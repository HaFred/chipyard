package dla.pe

import chisel3._
import chisel3.util._

// May31 TODO done
// 1. Get the PE Cluster work, fix with the RS+ fashion -Turn to GenTEST
// 2. Get the CSC data in work -CHECK
// 3. make sure the queue here is working so the dataStream are all well aligned
// 4. try to utilize the .lvy and dot to generate whole PE System diagram (NOT Necessary)

// TODO JUN 3
//  1 get the CSC weight & InAct solved, which one's format should be used?
//  2 Modify genTest or PEClusterTest or whatever aiming to the NoC level. Let's try the real RS+ dataReuse first
//  3 And then it is to do convert a partial sum to fullSum, take one conv layer from alexnet as golden model

class ProcessingElement(debug: Boolean) extends Module with PESizeConfig {
  val io: ProcessingElementIO = IO(new ProcessingElementIO)
  protected val peCtrl: ProcessingElementControl = Module(new ProcessingElementControl(debug = debug))
  peCtrl.suggestName("peCtrlModule")
  protected val peCtrlIO: ProcessingElementControlIO = peCtrl.io
  protected val pePad: ProcessingElementPad = Module(new ProcessingElementPad(debug = debug))
  pePad.suggestName("pePadModule")
  protected val pePadIO: ProcessingElementPadIO = pePad.io
  if (fifoEn) {             // f: connect the pePad with the topIO which communicate with testbench
    pePadIO.dataStream.inActIOs.adrIOs.data <> Queue(io.dataStream.inActIOs.adrIOs.data, fifoSize, flow = true, pipe = true)
    pePadIO.dataStream.inActIOs.dataIOs.data <> Queue(io.dataStream.inActIOs.dataIOs.data, fifoSize, flow = true, pipe = true)
    pePadIO.dataStream.weightIOs.adrIOs.data <> Queue(io.dataStream.weightIOs.adrIOs.data, fifoSize, flow = true, pipe = true)
    pePadIO.dataStream.weightIOs.dataIOs.data <> Queue(io.dataStream.weightIOs.dataIOs.data, fifoSize, flow = true, pipe = true)
  } else {
    pePadIO.dataStream.inActIOs <> io.dataStream.inActIOs
    pePadIO.dataStream.weightIOs <> io.dataStream.weightIOs
 }
  protected val inActAndWeightWFIOs = Seq(pePadIO.padWF.inActWriteFin, pePadIO.padWF.weightWriteFin)
  protected val inActAndWeightTopWFIOs = Seq(io.padWF.inActWriteFin, io.padWF.weightWriteFin)
  inActAndWeightWFIOs.zip(inActAndWeightTopWFIOs).foreach{case (x, y) => y <> x}
  io.padWF.pSumAddFin := pePadIO.padWF.pSumAddFin
  peCtrlIO.ctrlPad <> pePadIO.padCtrl
  peCtrlIO.ctrlTop.pSumEnqEn := io.topCtrl.pSumEnqEn
  io.topCtrl.calFinish := peCtrlIO.ctrlTop.calFinish
  peCtrlIO.ctrlTop.doLoadEn := io.topCtrl.doLoadEn // f: while debug, sent from testbench to top to CtrlIO
  protected val SPadWFSeq = Seq(inActAndWeightWFIOs.head.adrWriteFin, inActAndWeightWFIOs.head.dataWriteFin,
    inActAndWeightWFIOs.last.adrWriteFin, inActAndWeightWFIOs.last.dataWriteFin)
  protected val writeFinishWire: Bool = Wire(Bool())
  writeFinishWire.suggestName("inActAndWeightWFWire")
  protected val writeFinishRegVec: Seq[Bool] = Seq.fill(SPadWFSeq.length){RegInit(false.B)}
  writeFinishRegVec.head.suggestName("inActAdrWFReg")
  writeFinishRegVec(1).suggestName("inActDataWFReg")
  writeFinishRegVec(2).suggestName("weightAdrWFReg")
  writeFinishRegVec.last.suggestName("weightDataWFReg")
  for (i <- SPadWFSeq.indices) {
    writeFinishRegVec(i) := Mux(writeFinishWire, false.B, Mux(SPadWFSeq(i), true.B, writeFinishRegVec(i)))
  }
  writeFinishWire := writeFinishRegVec.reduce(_ && _) // when inAct and Weight Scratch Pads write finished
  io.topCtrl.writeFinish := writeFinishWire
  peCtrlIO.ctrlTop.writeFinish := writeFinishWire
  pePadIO.dataStream.ipsIO <> Queue(io.dataStream.ipsIO, fifoSize, flow = true, pipe = true)
  io.dataStream.opsIO <> Queue(pePadIO.dataStream.opsIO, fifoSize, flow = true, pipe = true)
  if (debug) {
    io.debugIO.peControlDebugIO <> peCtrlIO.debugIO
    io.debugIO.peSPadDebugIO <> pePadIO.debugIO
    io.debugIO.writeFinishRegVec <> writeFinishRegVec
  } else {
    io.debugIO <> DontCare
  }
}

class ProcessingElementControl(debug: Boolean) extends Module with MCRENFConfig {
  val io: ProcessingElementControlIO = IO(new ProcessingElementControlIO)
  // state machine, control the process of MAC
  // psIdle: wait for signal
  // psLoad: load input activations, weights, partial sums outside and read out output partial sum
  // psCal: do MAC computations
  protected val psIdle :: psLoad :: psCal :: Nil = Enum(3)
  protected val stateMac: UInt = RegInit(psIdle) // the state of the mac process
  stateMac.suggestName("PEStateReg")
  // fred: where pass to the MAC finish
  io.ctrlTop.calFinish := io.ctrlPad.fromTopIO.calFinish && stateMac === psCal
  io.ctrlPad.fromTopIO.pSumEnqEn := io.ctrlTop.pSumEnqEn
  io.ctrlPad.fromTopIO.doLoadEn := io.ctrlTop.doLoadEn // f: passed from line 41
  io.ctrlPad.doMACEn := stateMac === psCal
  switch (stateMac) {
    is (psIdle) {
      when (io.ctrlTop.doLoadEn) { // when there is any mac leaving
        stateMac := psLoad
      }
    }
    is (psLoad) {
      when (io.ctrlTop.writeFinish) { //after the pad receives the data, this signal from SPad useful for debugging peek
        stateMac := psCal
      }
    }
    is (psCal) {
      when (io.ctrlPad.fromTopIO.calFinish) {
        stateMac := psIdle
      }
    }
  }
  if (debug) {
    io.debugIO.peState := stateMac
    io.debugIO.doMACEnDebug := io.ctrlPad.doMACEn
  } else {
    io.debugIO <> DontCare
  }
}

class ProcessingElementPad(debug: Boolean) extends Module with MCRENFConfig with SPadSizeConfig with PESizeConfig {
  val io: ProcessingElementPadIO = IO(new ProcessingElementPadIO)
  protected def nextSPadInActAdr(): Unit = {
    sPad := padInActAdr
    inActAdrSPadReadEnReg := true.B
    inActDataSPadReadEnReg := false.B
    weightAdrSPadReadEnReg := false.B
    weightDataSPadReadEnReg := false.B
    weightDataSPadFirstRead := true.B // if need read a new column of input activation matrix, then true
  }
  protected def nextSPadInActData(): Unit = {
    sPad := padInActData
    inActAdrSPadReadEnReg := false.B
    inActDataSPadReadEnReg := true.B
    weightAdrSPadReadEnReg := false.B
    weightDataSPadReadEnReg := false.B
    weightDataSPadFirstRead := true.B // if need to read a new data, also a new weight matrix column
  }
  protected def nextSPadWeightAdr(): Unit = {
    sPad := padWeightAdr
    inActAdrSPadReadEnReg := false.B
    inActDataSPadReadEnReg := false.B
    weightAdrSPadReadEnReg := true.B
    weightDataSPadReadEnReg := false.B
  }
  protected def nextSPadWeightData(): Unit = {
    sPad := padWeightData1
    inActAdrSPadReadEnReg := false.B
    inActDataSPadReadEnReg := false.B
    weightAdrSPadReadEnReg := false.B
    weightDataSPadReadEnReg := true.B
  }
  protected def readOff(): Unit = {
    inActAdrSPadReadEnReg := false.B
    inActDataSPadReadEnReg := false.B
    weightAdrSPadReadEnReg := false.B
    weightDataSPadReadEnReg := false.B
  }
  protected def readFinish(): Unit = {
    sPad := padIdle
    inActDataSPadFirstReadReg := true.B
    inActMatrixColumnReg := 0.U
  }
  // reg, partial sum scratch pad
  protected val pSumSPad: PSumSPad = Module(new PSumSPad(debug = debug))
  pSumSPad.suggestName("pSumSPadModule")
  protected val inActAdrSPad: SPadAdrModule = Module(new SPadAdrModule(inActAdrSPadSize, inActAdrWidth))
  inActAdrSPad.suggestName("inActAdrSPadModule")
  protected val inActDataSPad: SPadDataModule = Module(new SPadDataModule(inActDataSPadSize, inActDataWidth, false))
  inActDataSPad.suggestName("inActDataSPadModule")
  protected val weightAdrSPad: WeightSPadAdrModule = Module(new WeightSPadAdrModule(weightAdrSPadSize, weightAdrWidth))
  weightAdrSPad.suggestName("weightAdrSPadModule")
  protected val weightDataSPad: SPadDataModule = Module(new SPadDataModule(weightDataSPadSize, weightDataWidth, true))
  weightDataSPad.suggestName("weightDataSPadModule")
  // get the IOs
  protected val pSumSPadIO: PSumSPadIO = pSumSPad.io
  protected val inActAdrSPadIO: SPadCommonModuleIO = inActAdrSPad.io
  protected val inActDataSPadIO: SPadCommonModuleIO = inActDataSPad.io
  protected val weightAdrSPadIO: SPadCommonModuleIO = weightAdrSPad.io
  protected val weightDataSPadIO: SPadCommonModuleIO = weightDataSPad.io
  // SPadToCtrl
  // several signals which can help to indicate the process
  protected val mightInActZeroColumnWire: Bool = Wire(Bool())
  protected val inActSPadZeroColumnReg: Bool = RegInit(false.B) // true, it is a zero column, then need read again
  protected val mightInActIdxIncWire: Bool = Wire(Bool())
  protected val mightWeightZeroColumnWire: Bool = Wire(Bool())
  protected val mightWeightIdxIncWire: Bool = Wire(Bool())
  protected val mightInActReadFinish: Bool = Wire(Bool())
  mightInActReadFinish.suggestName("mightInActReadFinish")
  /** true while weightMatrixData = 0*/
  protected val mightWeightReadFinish: Bool = Wire(Bool())
  /** true while weightAdr = 0, then it should turn to read new inActAdr */ // read whole weight adr vec, then inc inAct adr
  protected val mightWeightMatrixFinish: Bool = Wire(Bool())
  protected val psDataSPadIdxWire: UInt = Wire(UInt(log2Ceil(pSumDataSPadSize).W))
  // InActSPad
  protected val inActAdrIndexWire: UInt = Wire(UInt(inActAdrIdxWidth.W))
  protected val inActAdrDataWire: UInt = Wire(UInt(inActAdrWidth.W))
  inActAdrDataWire.suggestName("inActAdrData")
  protected val inActDataIndexWire: UInt = Wire(UInt(inActDataIdxWidth.W)) // use for address vector readEn
  protected val inActAdrSPadReadEnReg: Bool = RegInit(false.B)
  protected val inActDataSPadReadEnReg: Bool = RegInit(false.B)
  protected val inActAdrSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of inAct address SPad
  protected val inActDataSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of inAct data SPad
  protected val inActMatrixColumnReg: UInt = RegInit(0.U(inActAdrIdxWidth.W))
  protected val inActZeroColumnNumber: UInt = RegInit(0.U(inActAdrIdxWidth.W)) // use for get the right column number
  protected val inActDataSPadFirstReadReg: Bool = RegInit(true.B)
  inActDataSPadFirstReadReg.suggestName("inActFirstRead")
  protected val inActMatrixRowWire: UInt = Wire(UInt(cscCountWidth.W))
  inActMatrixRowWire.suggestName("inActMatrixRowWire")
  protected val inActMatrixDataWire: UInt = Wire(UInt(cscDataWidth.W))
  inActMatrixDataWire.suggestName("inActMatrixDataWire")
  // WeightSPad
  protected val weightAdrIndexWire: UInt = Wire(UInt(weightAdrIdxWidth.W))  // f: for spad
  protected val weightAdrDataWire: UInt = Wire(UInt(weightAdrWidth.W))  // f: for pe
  protected val weightDataIndexWire: UInt = Wire(UInt(weightDataIdxWidth.W)) // use for address vector readEn
  protected val weightAdrSPadReadEnReg: Bool = RegInit(false.B)
  protected val weightDataSPadReadEnReg: Bool = RegInit(false.B)
  protected val weightAdrSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of weight address SPad
  protected val weightDataSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of weight data SPad
  protected val weightMatrixDataReg: UInt = Wire(UInt(cscDataWidth.W))
  protected val weightDataSPadFirstRead: Bool = RegInit(true.B)
  protected val weightDataIdxMuxWire: Bool = Wire(Bool()) // false, then means need to read the first column of weight Matrix
  protected val weightAdrSPadReadIdxWire: UInt = Wire(UInt(cscCountWidth.W))
  protected val weightDataIdxEnWire: Bool = Wire(Bool()) // true then assign a special one for data index
  protected val weightAdrIdxEnWire: Bool = Wire(Bool()) // true then assign a special one for data index
  protected val weightMatrixReadFirstColumn: Bool = Wire(Bool())
  // pSumSPadIO
  protected val productReg: UInt = RegInit(0.U(psDataWidth.W))
  protected val pSumSPadLoadReg: UInt = RegInit(0.U(psDataWidth.W))
  protected val pSumSPadLoadWire: UInt = Wire(UInt(psDataWidth.W))
  // State Machine
  // padIdle: the pad is idle, and if received valid signal from control, then read and send data to mac
  // padInActAdr: read the input activation address
  // padInActData: read the input activation data
  // padWeightAdr: read the weight address
  // padWeightData1: read the weight data
  // padWeightData2: wait one cycle as SRAM
  // padMpy: wait for mac computation
  // padWriteBack: write the partial sum back
  protected val padIdle :: padInActAdr :: padInActData :: padWeightAdr :: padWeightData1 :: padWeightData2 :: padMpy :: padWriteBack :: Nil = Enum(8)
  protected val sPad: UInt = RegInit(padIdle)
  sPad.suggestName("PESPadStateReg")
  protected val padEqIA: Bool = Wire(Bool())
  protected val padEqID: Bool = Wire(Bool())
  protected val padEqWA: Bool = Wire(Bool())
  protected val padEqMpy: Bool = Wire(Bool())
  protected val padEqWB: Bool = Wire(Bool())
  padEqIA := sPad === padInActAdr
  padEqMpy := sPad === padMpy
  padEqWB := sPad === padWriteBack
  padEqWA := sPad === padWeightAdr
  padEqID := sPad === padInActData
  weightMatrixReadFirstColumn := inActMatrixRowWire === 0.U
  protected val weightMatrixRowReg: UInt = Wire(UInt(cscCountWidth.W))
  weightMatrixRowReg.suggestName("weightMatrixRowReg")
  protected val SPadSeq = Seq(inActAdrSPadIO, inActDataSPadIO, weightAdrSPadIO, weightDataSPadIO)

  // Connections
  SPadSeq.map(_.ctrlPath.writeEn := io.padCtrl.fromTopIO.doLoadEn) // f: 4 inact&weight IO's enable connect to the doLoadEn
  io.padCtrl.fromTopIO.writeFinish := DontCare

  // Input activation Address Scratch Pad
  inActAdrSPadIO.dataPath.writeInData <> io.dataStream.inActIOs.adrIOs
  inActAdrIndexWire := inActAdrSPadIO.dataPath.columnNum           // f: those connection is in SPadAdrModule
  inActAdrDataWire := inActAdrSPadIO.dataPath.readOutData
  io.padWF.inActWriteFin.adrWriteFin := inActAdrSPadIO.ctrlPath.writeFin  // output := output, in to out connection
  inActAdrSPadIO.ctrlPath.readEn := inActAdrSPadReadEnReg
  inActAdrSPadIO.ctrlPath.readInIdx := DontCare
  inActAdrSPadIO.ctrlPath.indexInc := inActAdrSPadIdxIncWire
  inActAdrSPadIO.ctrlPath.readInIdxEn := DontCare                /** // f: finish these 5 input ports for SPadIO.ctrlPath assignment. Inputs has to be specified or put as DontCare*/

  // Input activation Data Scratch Pad
  inActDataSPadIO.dataPath.writeInData <> io.dataStream.inActIOs.dataIOs
  inActDataIndexWire := inActDataSPadIO.dataPath.columnNum
  inActMatrixDataWire := inActDataSPadIO.dataPath.readOutData(cscDataWidth + cscCountWidth - 1, cscCountWidth)
  inActMatrixRowWire := inActDataSPadIO.dataPath.readOutData(cscCountWidth - 1, 0)    /** for each row? */ // I think it is indexing UInt, first Count width for RowWire, and below DataWidth for DataWire
  io.padWF.inActWriteFin.dataWriteFin := inActDataSPadIO.ctrlPath.writeFin
  inActDataSPadIO.ctrlPath.readEn := inActDataSPadReadEnReg
  inActDataSPadIO.ctrlPath.readInIdx := inActAdrDataWire
  inActDataSPadIO.ctrlPath.indexInc := inActDataSPadIdxIncWire
  inActDataSPadIO.ctrlPath.readInIdxEn := DontCare

  // Weight Address Scratch Pad
  weightAdrSPadIO.dataPath.writeInData <> io.dataStream.weightIOs.adrIOs
  weightAdrIndexWire := weightAdrSPadIO.dataPath.columnNum
  weightAdrDataWire := weightAdrSPadIO.dataPath.readOutData
  io.padWF.weightWriteFin.adrWriteFin := weightAdrSPadIO.ctrlPath.writeFin
  weightAdrSPadIO.ctrlPath.readEn := weightAdrSPadReadEnReg
  weightAdrSPadIO.ctrlPath.readInIdx := weightAdrSPadReadIdxWire // the weight address SPad's columns corresponds to
                                                          // the inAct address SPad's rows, and it takes one clock cycle
                                                          // for the reg inside SPad to change the index it need
  weightAdrSPadIO.ctrlPath.indexInc := weightAdrSPadIdxIncWire
  weightAdrSPadIO.ctrlPath.readInIdxEn := weightAdrIdxEnWire

  // Weight Data Scratch Pad
  weightDataSPadIO.dataPath.writeInData <> io.dataStream.weightIOs.dataIOs
  weightDataIndexWire := weightDataSPadIO.dataPath.columnNum
  weightMatrixDataReg := weightDataSPadIO.dataPath.readOutData(cscDataWidth + cscCountWidth - 1, cscCountWidth)
  weightMatrixRowReg := weightDataSPadIO.dataPath.readOutData(cscCountWidth - 1, 0)
  io.padWF.weightWriteFin.dataWriteFin := weightDataSPadIO.ctrlPath.writeFin
  weightDataSPadIO.ctrlPath.readEn := inActDataSPadReadEnReg
  weightDataSPadIO.ctrlPath.readInIdx := Mux(weightMatrixReadFirstColumn, 0.U, weightAdrDataWire)
  weightDataSPadIO.ctrlPath.indexInc := weightDataSPadIdxIncWire
  weightDataSPadIO.ctrlPath.readInIdxEn := weightDataIdxEnWire

  // Partial Sum Scratch Pad
  protected val pSumLoadStateReg: Bool = RegInit(false.B) // true when load PSum out  f: or in?
  pSumLoadStateReg.suggestName("pSumLoadStateReg") /** cycle synchronous to update pSumLoadStateReg */
  protected val pSumAddResultWire: UInt = Mux(pSumLoadStateReg, pSumSPadLoadWire, pSumSPadLoadReg) +
    Mux(pSumLoadStateReg, io.dataStream.ipsIO.bits, productReg) // to save adder, the load state determine PSum load in or accumulate in PE
  protected val pSumResultWire: UInt = Mux(padEqWB, pSumAddResultWire, 0.U)
  protected val pSumPadReadIdxReg: UInt = RegInit(0.U(log2Ceil(pSumDataSPadSize).W))
  pSumPadReadIdxReg.suggestName("pSumPadReadIdxReg")
  protected val pSumPadReadIdxIncWire: Bool = Wire(Bool())
  pSumPadReadIdxIncWire.suggestName("pSumPadReadIdxIncWire")
  protected val pSumPadReadWrap: Bool = Wire(Bool())
  pSumPadReadWrap.suggestName("pSumPadReadWrap")
  pSumLoadStateReg := Mux(io.padWF.pSumAddFin, false.B,
    Mux(io.padCtrl.fromTopIO.pSumEnqEn, true.B, pSumLoadStateReg))  /** if pSumAddFin, don't load in, if enqueEn load in, other stays*/
  protected val pSumReadOutValid: Bool = io.dataStream.ipsIO.valid && pSumLoadStateReg
  // top connections
  io.padWF.pSumAddFin := pSumPadReadWrap && pSumLoadStateReg // when need read and wrap
  io.dataStream.ipsIO.ready := io.dataStream.opsIO.ready && pSumLoadStateReg  /** when next PE gives me opsIO.ready true, my load in state is also true*/
  /** the PSumSPad read index only increases when io.dataStream.opsIO gets the updated PSum*/
  pSumPadReadIdxIncWire := io.dataStream.opsIO.fire()
  // only when need enqueue, and pSumSPadIO has read out data, and io.dataStream.ops has received data,
  // then increase read index
  io.dataStream.opsIO.bits := pSumAddResultWire
  /** io.dataStream.opsIO.valid when it reads one PSum from PSumSPad
    * AND it can get another PSum from io.dataStream.ipsIO, then can add them together
    * and get the io.dataStream.opsIO.bits*/
  io.dataStream.opsIO.valid := pSumSPadIO.dataPath.opsIO.fire() && pSumReadOutValid
  // once ask for Enq, pSumSPadIO will wait for io.dataStream.ipsIO.valid, then read out data,
  // and add the data read out from pSumSPadIO with io.dataStream.ipsIO.bit.
  // io.dataStream.opsIO.valid will be true when those two addends valid.
  // The read address will increase after io.dataStream.ops.ready is true, as well as io.dataStream.ips.ready.
  // Because we have Queue, so we don't need to worry about the timing of this combination logic.
  pSumSPadIO.dataPath.ipsIO.valid := padEqWB
  pSumSPadIO.dataPath.ipsIO.bits := pSumResultWire // only calculate need write back
  pSumSPadLoadWire := pSumSPadIO.dataPath.opsIO.bits // for emergence use
  pSumSPadLoadReg := pSumSPadLoadWire // used in calculating
  // when ips valid and Enabled, then need read out PSum
  pSumSPadIO.dataPath.opsIO.ready := padEqMpy || pSumReadOutValid
  pSumSPadIO.ctrlPath.readIdx := pSumPadReadIdxReg
  pSumSPadIO.ctrlPath.writeIdx := psDataSPadIdxWire // only calculate need write back
  pSumPadReadIdxReg := Mux(sPad === padWeightData2, psDataSPadIdxWire,
    Mux(pSumPadReadWrap, 0.U, Mux(pSumPadReadIdxIncWire, pSumPadReadIdxReg + 1.U, pSumPadReadIdxReg)))      /** is weightData2? is it Wrap? is IdxIncWire?*/
  /** the first half of pSumPadReadWrap is needed when doing mac for getting pSum
    * another half is needed when reading all pSum out*/
  pSumPadReadWrap := (padEqID && mightInActReadFinish) || (padEqWB && mightInActReadFinish) ||   /** if the any of these true, or if IdxReg reach end, Wrap will be true & the IdxReg will be reset*/
    (pSumPadReadIdxIncWire && pSumPadReadIdxReg === (M0*E*N0*F0 -1 ).U) // no need to use mux FIXME

    // SPadToCtrl
  mightInActZeroColumnWire := inActAdrDataWire === inActZeroColumnCode.U         /** f: where CSC works*/
  mightWeightZeroColumnWire := weightAdrDataWire === weightZeroColumnCode.U
  mightInActIdxIncWire := inActAdrDataWire === (inActDataIndexWire + 1.U)
  mightWeightIdxIncWire := weightAdrDataWire === (weightDataIndexWire + 1.U) || mightWeightReadFinish // or meet finish signal
  mightInActReadFinish := inActMatrixDataWire === 0.U && !inActDataSPadFirstReadReg
  mightWeightReadFinish := weightMatrixDataReg === 0.U && !weightDataSPadFirstRead  /** how will data === 0.U ? It is coz 0 at the end*/
  mightWeightMatrixFinish := weightAdrDataWire === 0.U
  inActAdrSPadIdxIncWire := (padEqIA && mightInActZeroColumnWire) || (((padEqWA && mightWeightZeroColumnWire) ||      /** indicates inAct adr increment*/
    (padEqWB && mightWeightIdxIncWire)) && mightInActIdxIncWire) || (mightInActReadFinish && sPad =/= 0.U)  /** when InAct at zero col, skip, when InAct not zero but weight zero, skip, or InActReadFinish and sPad not Idle, skip*/
  weightAdrSPadIdxIncWire := (padEqMpy || sPad === padWeightData1) && mightWeightZeroColumnWire // FIXME: should add a state
  // if first read, then keep the read index of zero
  inActDataSPadIdxIncWire := (padEqIA && !mightInActZeroColumnWire && !inActDataSPadFirstReadReg) ||
    (((padEqWA && mightWeightZeroColumnWire) ||
      (padEqWB && mightWeightIdxIncWire)) && !mightInActIdxIncWire) || (mightInActReadFinish && sPad =/= 0.U)
  weightDataSPadIdxIncWire := (padEqWA && !mightWeightZeroColumnWire && !weightDataSPadFirstRead) ||
    (padEqWB && !mightWeightIdxIncWire) // when first read, ask Weight Address Scratch Pad for data index
  weightAdrIdxEnWire := (padEqID || padEqWA) && weightDataSPadFirstRead // read the start and end index from address SPad
  // then it can read the start index in weightDataSPadIO, the end index of that will be read otherwise
  weightDataIdxMuxWire := padEqID && weightDataSPadFirstRead && !weightMatrixReadFirstColumn
  weightAdrSPadReadIdxWire := Mux(weightDataIdxMuxWire, inActMatrixRowWire - 1.U, inActMatrixRowWire)
  weightDataIdxEnWire := padEqWA && weightDataSPadFirstRead && !mightWeightZeroColumnWire
  // fred: pad-level calFinish
  io.padCtrl.fromTopIO.calFinish := mightInActReadFinish && sPad =/= 0.U
  psDataSPadIdxWire := weightMatrixRowReg + inActMatrixColumnReg * M0.U
  switch (sPad) {
    is (padIdle) {
      when(io.padCtrl.doMACEn) {
        nextSPadInActAdr()
      }
    }
    is (padInActAdr) {
      when (mightInActZeroColumnWire) { // then it is a zero column
        nextSPadInActAdr()
        inActSPadZeroColumnReg := true.B
        inActZeroColumnNumber := inActZeroColumnNumber + 1.U
      } .otherwise {
        nextSPadInActData()
      }
    }
    is (padInActData) {
      when (mightInActReadFinish) {
        readFinish()
      } .otherwise {
        nextSPadWeightAdr()
      }
    }
    is (padWeightAdr) {
      inActDataSPadFirstReadReg := false.B
      when (mightWeightZeroColumnWire) { // need to get next inAct
        when (mightInActIdxIncWire) { // if have read all elements in current inAct Matrix column
          nextSPadInActAdr()
          when (inActSPadZeroColumnReg) {
            inActSPadZeroColumnReg := false.B
            inActMatrixColumnReg := inActMatrixColumnReg + 1.U + inActZeroColumnNumber
            inActZeroColumnNumber := 0.U
          } .otherwise {
            inActMatrixColumnReg := inActMatrixColumnReg + 1.U
          }
        } .otherwise { // still some elements in current inAct Matrix column
          nextSPadInActData()
        }
      } .otherwise { // then it isn't a zero column, can do MAC
        nextSPadWeightData()
      }
    }
    is (padWeightData1) {
      sPad := padWeightData2
    }
    is (padWeightData2) {
      sPad := padMpy
      readOff()
    }
    is (padMpy) {
      sPad := padWriteBack
      productReg :=  weightMatrixDataReg * inActMatrixDataWire
      // then load pSum
      //pSumSPadLoadReg := psDataSPadReg(pSumPadReadIdxReg)
    }
    is (padWriteBack) {
      // then write back pSum
      //psDataSPadReg(pSumPadWriteIdxWire) := pSumResultWire //update the partial sum
      when (mightInActReadFinish) { //f: con finish
        readFinish()
      } .otherwise { // then haven't done all the MAC operations
        when (mightWeightIdxIncWire || mightWeightMatrixFinish) { // finished read current weight data Matrix column
          when (mightInActIdxIncWire || mightWeightMatrixFinish) { // finished read current inAct data Matrix column
            nextSPadInActAdr()   // f: con3, where both weigh & inAct need to increase
            when (inActSPadZeroColumnReg) { // f: when next column is 0 column, while in con3, update 0colReg
              inActSPadZeroColumnReg := false.B
              inActMatrixColumnReg := inActMatrixColumnReg + 1.U + inActZeroColumnNumber
              inActZeroColumnNumber := 0.U
            } .otherwise {
              inActMatrixColumnReg := inActMatrixColumnReg + 1.U
            }
          } .otherwise {
            nextSPadInActData()  // con2, since the InAct col haven't finish, fetch act data
          }
        } .otherwise { // con1, weight row haven't finished either, fetch weight data
          nextSPadWeightData()
          weightDataSPadFirstRead := false.B // as it has been read current weight matrix column
        }
      }
    }
  }

  if (debug) {
    io.debugIO.inActMatrixColumn := inActMatrixColumnReg
    io.debugIO.inActMatrixData := inActMatrixDataWire
    io.debugIO.inActMatrixRow := inActMatrixRowWire
    io.debugIO.inActAdrIdx := inActAdrIndexWire  // column number
    io.debugIO.inActAdrInc := inActAdrSPadIdxIncWire
    io.debugIO.inActDataInc := inActDataSPadIdxIncWire
    io.debugIO.weightMatrixData := weightMatrixDataReg
    io.debugIO.weightMatrixRow := weightMatrixRowReg
    io.debugIO.weightAdrSPadReadOut := weightAdrDataWire
    io.debugIO.productResult := productReg
    io.debugIO.pSumResult := pSumResultWire
    io.debugIO.pSumLoad := pSumSPadLoadReg
    io.debugIO.weightAdrInIdx := weightAdrSPadReadIdxWire
    io.debugIO.sPadState := sPad
    io.debugIO.pSumReadIdx := pSumPadReadIdxReg
    io.debugIO.pSumPadReadIdx := pSumPadReadIdxReg // f: for debug
    io.debugIO.inActDataSliding := DontCare
    io.debugIO.inActDataSlidingFire := DontCare
    io.debugIO.futureLBStart := DontCare
    io.debugIO.inActDataIndex := DontCare
    io.debugIO.inActAdrData := DontCare
  }else {
    io.debugIO <> DontCare
  }
}
