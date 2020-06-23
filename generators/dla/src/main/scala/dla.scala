package dla

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._ // LazyModule
import freechips.rocketchip.rocket._ // cache
import icenet._
import freechips.rocketchip.config.{Config}

// f:
// this RoCC use DMA to read the main mem into its on-chip buffer, and write the on-chip buffer's content into the main mem
// if we are building own DMA without this iceDMA, might need the MMIO Reg to control DMA
class RoCCIceDMA(opcode: OpcodeSet, val regBufferNum: Int = 1024)(implicit p: Parameters) extends LazyRoCC(opcode) {
  val nXacts = 4 // DMA channel
  val outFlits = 32 // DMA buffer size
  val maxBytes = 64 // DMA foreach TL
  val dmaReader = LazyModule(new StreamReader(nXacts, outFlits, maxBytes)(p)) // used to read DMA
  val dmaWriter = LazyModule(new StreamWriter(nXacts, maxBytes)(p)) //
  tldmaNode :=* dmaReader.node // query type connection
  tldmaNode :=* dmaWriter.node
  override lazy val module = new LazyRoCCModuleImp(this){
    // define the data storing buffer
    val buffer = RegInit(VecInit(Seq.fill(regBufferNum)(0.U(64.W))))
    val i = RegInit(0.U(log2Ceil(regBufferNum+1).W))
    // decoding vars
    val len = RegInit(0.U(64.W))
    val nbytes = len << 3.U
    val baseAdr = RegInit(0.U(64.W))
    val direction = RegInit(0.U(3.W))  // f: if direction got 0 means wrong
    val busy = RegInit(false.B)
    // DMA Read
    val dmaReaderIO = dmaReader.module.io // icenet DMA read port
    val canRead = busy && (direction===1.U)
    val rRestReqValid = RegInit(false.B)
    // DMA Write
    val dmaWriterIO = dmaWriter.module.io
    val canWrite = busy && (direction===2.U)
    val wRestReqValid = RegInit(false.B)

    // RoCC instructions decoding
    io.cmd.ready := !busy
    io.busy := busy
    when(io.cmd.fire()){ // when fire the rocket core will send ins
      busy := true.B
      i := 0.U
      rRestReqValid := false.B
      wRestReqValid := false.B
      direction := MuxCase(0.U, Seq(
        (io.cmd.bits.inst.funct===0.U) -> 1.U,
        (io.cmd.bits.inst.funct===1.U) -> 2.U,
        (io.cmd.bits.inst.funct===2.U) -> 3.U,
        (io.cmd.bits.inst.funct===3.U) -> 4.U
      ))
      len := io.cmd.bits.rs1
      baseAdr := io.cmd.bits.rs2
    }
    // DMA Read, when IO fire, make request valid
    when(dmaReaderIO.req.fire()) {rRestReqValid := true.B}
    dmaReaderIO.req.valid := canRead && !rRestReqValid
    dmaReaderIO.req.bits.address := baseAdr
    dmaReaderIO.req.bits.length := nbytes
    dmaReaderIO.req.bits.partial := false.B
    dmaReaderIO.out.ready := (i < len) && canRead  // assume out.ready is inputted as this statement
    buffer(i) := dmaReaderIO.out.bits.data
    when(dmaReaderIO.out.fire()){
      i := i + 1.U
    }
    dmaReaderIO.resp.ready := dmaReaderIO.resp.valid // for the resp, ALA I occur valid output, I assume received ready
    when(busy && i === len && canRead){
      busy := false.B
    }
    // DMA Write, same fashion as Read
    when(dmaWriterIO.req.fire()){wRestReqValid := true.B}
    dmaWriterIO.req.valid := canWrite && !wRestReqValid
    dmaWriterIO.req.bits.address := baseAdr
    dmaWriterIO.req.bits.length := nbytes
    dmaWriterIO.in.valid := (i < len) && canWrite // assume in.valid is inputted as this statement
    dmaWriterIO.in.bits.data := buffer(i)
    when(dmaWriterIO.in.fire()){
      i := i + 1.U
    }
    dmaWriterIO.resp.ready := dmaWriterIO.resp.valid
    when(dmaWriterIO.resp.valid && canWrite) {
      busy := false.B
    }

    // access mem with cache controller IO
    val memReq = io.mem.req
    val memResp = io.mem.resp
    val s_idle :: s_op :: Nil = Enum(2)
    val state = RegInit(s_idle)
    val enCacheR = busy && (direction === 3.U) && (i < len)
    val enCacheW = busy && (direction === 4.U) && (i < len)

    memReq.valid := (enCacheR || enCacheW) && (state === s_idle)
    memReq.bits.cmd := Mux(enCacheW, canWrite, canRead)
    memReq.bits.addr := baseAdr
    memReq.bits.size := log2Ceil(64).U
    memReq.bits.data := buffer(i)
    when(memReq.fire()) {state := s_op}
    // read cache to buffer
    when(state === s_op && enCacheR && memResp.valid) {
      baseAdr := baseAdr + 8.U
      i := i + 1.U
      buffer(i) := memResp.bits.data
      state := s_idle
    }
    // write cache from buffer
    when(state === s_op && enCacheW) {
      baseAdr := baseAdr + 8.U
      i := i + 1.U
      state := s_idle
    }
    when (busy && !enCacheR && !enCacheW && direction > 2.U) {busy := false.B}
  }
}

class WithRoCCIceDMA extends Config((site, here, up) => {
  case BuildRoCC => Seq(
    (p:Parameters) => {
      val regBufferNum = 4096 // Reg width
      val roccdma = LazyModule (new RoCCIceDMA(OpcodeSet.all, regBufferNum)(p))
      roccdma
    }
  )
})