package dla.tests

import chisel3._
import chisel3.tester._
import org.scalatest._
import dla.pe._
//import dla.tests.GenOnePETestData
import freechips.rocketchip.tile._

import scala.math.pow
import scala.util.Random

class DMATester extends FlatSpec with ChiselScalatestTester with Matchers
  with SPadSizeConfig with MCRENFConfigRS with PESizeConfig {

  private val pSumSPadSize = M0*E0*F*N0
  private val inWeightAdr = Seq(2, 5, weightZeroColumnCode, 6, 7, weightZeroColumnCode, 9, 12, 0)  /** it means starting from the col1, the index of its first col, but the number of elements seem to be determined by # of elements of count vec */
  private val inWeightData = Seq(1, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 0) // zero column between 15 & 18, 24 & 27
  private val inWeightCount = Seq(1, 2, 0, 1, 3, 2, 3, 1, 3, 0, 1, 2, 0)
  private val inInActAdr = Seq(6, 10, 13, 0)
  private val inInActData = Seq(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 0)
  private val inInActCount = Seq(1, 3, 4, 6, 7, 9, 2, 5, 6, 8, 0, 5, 7, 0)
  private val outPSum = Seq(282, 318, 330, 132, 348, 606, 486, 432, 0, 336, 210, 384, 0, 14, 42, 324, 702, 1306, 924, 576, 0, 546, 0, 624)

  private val MAXSIZE = 4096
  private val testReadIn = new Array[Long](MAXSIZE)
  private val testWriteOut = new Array[Long](MAXSIZE)
//  var length: Long = 0
  private val nTests: Int = 10
//  val a, b: Long = 0
//  private val size = Seq(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 2560, 3072, 3584, 4095) // N=16
  private val numDMATest = 100
  private val numDataVarTest = 16
  private val dmaR = new Array[Long](numDMATest)
  private val dmaW = new Array[Long](numDMATest)

  /** this test is for the non-toeplitz InAct storing
   * outPSum is
   * 282	348	0	  0	  702	  0
   * 318	606	336	14	1306	546
   * 330	486	210	42	924	  0
   * 132	432	384	324	576	  624
   * */

  /** the inWeight matrix is
   * 0   6   0   0   0   0   0   27
   * 1   9   0   0   0   21  0   30
   * 3   0   15  0   0   0   0   33
   * 0   12  0   0   18  24  0   0
   * */
  /** the inInAct matrix with sliding is
   *  0   0   22
   *  2   0   0
   *  0   14  0
   *  4   0   0
   *  6   0   0
   *  0   16  24
   *  8   18  0
   *  10  0   26
   *  0	  20  0
   *  12  0   0
   * */
  /** the inInAct Toeplitz matrix used for software golden model is
   *  0   0   0   14   22  0
   *  2   4   0   0    0   0
   *  0   6   14  0    0   0
   *  4   0   0   16   0   24
   *  6   8   0   18   0   0
   *  0   10  16  0    24  26
   *  8   0   18  20   0   0
   *  10  12  0   0    26  0
   * */

  behavior of "test the DMA of Icenet"

//  it should "work for IceDMA module" in {
//    test(new RoCCIceDMA(OpcodeSet.custom0, 4096)) { theDMA =>
//      val theTopIO = theDMA.module.io
//      val theClock = theDMA.module.clock
//      var t = 0
//      for (testIdx <- 0 until numDataVarTest) {
//        println(s"--------------- $testIdx-th round test -----------")
//        for (i <- 0 until numDMATest) {
//          println(s"----- the i = $i")
//          length = size(testIdx)
//          // init testReadIn
//          for (j <- 0 until length) testReadIn(j) = (i + 1) * (j + 1)
//          println(s"----- the length = $length")
//          theClock.step()
//          println(s"--------------- $t-th clock cycle -----------")
//          t = t + 1
//          theTopIO.cmd.fire().poke(true.B) // or consider divide the fire
//          theClock.step()
//          println(s"--------------- $t-th clock cycle -----------")
//          theTopIO.cmd.bits.inst.funct.poke(0.U)
//          theTopIO.cmd.bits.rs1.poke(length.U)
//          theTopIO.cmd.bits.rs2.poke(testReadIn.U)  // todo fix this with the base adr
////          theClock.step()
////          println(s"--------------- $t-th clock cycle -----------")
//          while (current =/= true.B) {
//            theClock.step()
//            println(s"--------------- $t-th clock cycle -----------")
//            println(s"----- readInVal  =  ${theTopIO.debugIO.outdata.peek()}")
//          }
//
//
//
//
//
//        }
//      }
//    }
//  }
}


class ProcessingElementEmitVerilogTest extends FlatSpec with ChiselScalatestTester{


  val verilogString = (new chisel3.stage.ChiselStage).emitVerilog(new ProcessingElementPad(false))
//    println(verilogString)
//  private def PEScratchPadWriteIn(inInActAdr: Seq[Int], inInActData: Seq[Int], inWeightAdr: Seq[Int],
//                                  inWeightData: Seq[Int], topModule: ProcessingElementPad): Any = {
//    val theTopSPadIO = topModule.io.dataStream
//    val theClock = topModule.clock
//    topModule.io.padCtrl.doMACEn.poke(false.B)
//    inActAndWeightReadInFuc(inInActAdr, inInActData, inWeightAdr, inWeightData, theTopSPadIO, theClock, topModule.io.padWF)
//  }
}
