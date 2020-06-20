package chipyard

import chisel3._

import scala.collection.mutable.{ArrayBuffer}

import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.diplomacy.{LazyModule}
import freechips.rocketchip.util.{ResetCatchAndSync}
import chipyard.config.ConfigValName._
import chipyard.iobinders.{IOBinders, TestHarnessFunction, IOBinderTuple}

import barstools.iocell.chisel._
import chipsalliance.rocketchip.config.Config
import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts.HasInterruptSources
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc, RegisterRouter, RegisterRouterParams}
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile.{BuildRoCC, LazyRoCC, LazyRoCCModuleImp, OpcodeSet}
import icenet._

case object BuildSystem extends Field[Parameters => LazyModule]((p: Parameters) => LazyModule(new DigitalTop()(p)))

/**
  * Chipyard provides three baseline, top-level reset schemes, set using the
  * [[GlobalResetSchemeKey]] in a Parameters instance. These are:
  *
  * 1) Synchronous: The input coming to the chip is synchronous to the provided
  *    clocks and will be used without modification as a synchronous reset.
  *    This is safe only for use in FireSim and SW simulation.
  *
  * 2) Asynchronous: The input reset is asynchronous to the input clock, but it
  *    is caught and synchronized to that clock before it is dissemenated.
  *    Thus, downsteam modules will be emitted with synchronously reset state
  *    elements.
  *
  * 3) Asynchronous Full: The input reset is asynchronous to the input clock,
  *    and is used globally as an async reset. Downstream modules will be emitted
  *    with asynchronously reset state elements.
  *
  */
sealed trait GlobalResetScheme {
  def pinIsAsync: Boolean
}
sealed trait HasAsyncInput { self: GlobalResetScheme =>
  def pinIsAsync = true
}

sealed trait HasSyncInput { self: GlobalResetScheme =>
  def pinIsAsync = false
}

case object GlobalResetSynchronous extends GlobalResetScheme with HasSyncInput
case object GlobalResetAsynchronous extends GlobalResetScheme with HasAsyncInput
case object GlobalResetAsynchronousFull extends GlobalResetScheme with HasAsyncInput
case object GlobalResetSchemeKey extends Field[GlobalResetScheme](GlobalResetSynchronous)


/**
 * The base class used for building chips. This constructor instantiates a module specified by the BuildSystem parameter,
 * named "system", which is an instance of DigitalTop by default. The default clock and reset for "system" are set by two
 * wires, "systemClock" and "systemReset", which are intended to be driven by traits mixed-in with this base class.
 */
abstract class BaseChipTop()(implicit val p: Parameters) extends RawModule with HasTestHarnessFunctions {

  // A publicly accessible list of IO cells (useful for a floorplanning tool, for example)
  val iocells = ArrayBuffer.empty[IOCell]
  // A list of functions to call in the test harness
  val harnessFunctions = ArrayBuffer.empty[TestHarnessFunction]
  // The system clock
  // These are given so that IOCell can use DataMirror and generate ports with
  // the right flow (Input/Output)
  val systemClock = Wire(Input(Clock()))
  val systemReset = Wire(Input(Reset()))

  // The system module specified by BuildSystem
  val lSystem = p(BuildSystem)(p).suggestName("system")
  val system = withClockAndReset(systemClock, systemReset) { Module(lSystem.module) }

  // Call all of the IOBinders and provide them with a default clock and reset
  withClockAndReset(systemClock, systemReset) {
    // Call each IOBinder on both the lazyModule instance and the module
    // instance. Generally, an IOBinder PF should only be defined on one, so
    // this should not lead to two invocations.
    val (_ports, _iocells, _harnessFunctions) = p(IOBinders).values.flatMap(f => f(lSystem) ++ f(system)).unzip3
    // We ignore _ports for now...
    iocells ++= _iocells.flatten
    harnessFunctions ++= _harnessFunctions.flatten
  }

}

/**
 * A simple clock and reset implementation that punches out clock and reset ports with the same
 * names as the implicit clock and reset for standard Module classes. Three basic reset schemes 
 * are provided. See [[GlobalResetScheme]].
 */
trait HasChipTopSimpleClockAndReset { this: BaseChipTop =>

  val (clock, systemClockIO) = IOCell.generateIOFromSignal(systemClock, Some("iocell_clock"))
  val (reset, systemResetIO) = p(GlobalResetSchemeKey) match {
    case GlobalResetSynchronous  =>
      IOCell.generateIOFromSignal(systemReset, Some("iocell_reset"))
    case GlobalResetAsynchronousFull =>
      IOCell.generateIOFromSignal(systemReset, Some("iocell_reset"), abstractResetAsAsync = true)
    case GlobalResetAsynchronous =>
      val asyncResetCore = Wire(Input(AsyncReset()))
      systemReset := ResetCatchAndSync(systemClock, asyncResetCore.asBool)
      IOCell.generateIOFromSignal(asyncResetCore, Some("iocell_reset"), abstractResetAsAsync = true)
  }

  iocells ++= systemClockIO
  iocells ++= systemResetIO

  // Add a TestHarnessFunction that connects clock and reset
  harnessFunctions += { (th: TestHarness) => {
    // Connect clock; it's not done implicitly with RawModule
    clock := th.clock
    // Connect reset; it's not done implicitly with RawModule
    // Note that we need to use dutReset, not harnessReset
    reset := th.dutReset
    Nil
  } }

}

class ChipTop()(implicit p: Parameters) extends BaseChipTop()(p)
  with HasChipTopSimpleClockAndReset
  
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
    val direction = RegInit(0.U(3.W))
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
    dmaWriterIO.in.bits := buffer(i)
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
