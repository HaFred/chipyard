circuit nonToeplitzProcessingElement :
  module ProcessingElementControl :
    input clock : Clock
    input reset : UInt<1>
    output io_ctrlPad_doMACEn : UInt<1>
    output io_ctrlPad_fromTopIO_pSumEnqEn : UInt<1>
    output io_ctrlPad_fromTopIO_doLoadEn : UInt<1>
    input io_ctrlPad_fromTopIO_writeFinish : UInt<1>
    input io_ctrlPad_fromTopIO_calFinish : UInt<1>
    input io_ctrlTop_pSumEnqEn : UInt<1>
    input io_ctrlTop_doLoadEn : UInt<1>
    output io_ctrlTop_calFinish : UInt<1>
    input io_ctrlTop_writeFinish : UInt<1>
    output io_debugIO_peState : UInt<2>
    output io_debugIO_doMACEnDebug : UInt<1>
  
    reg PEStateReg : UInt<2>, clock with :
      reset => (UInt<1>("h0"), PEStateReg) @[ProcessingElement.scala 75:41]
    node _T = eq(PEStateReg, UInt<2>("h2")) @[ProcessingElement.scala 78:70]
    node _T_1 = and(io_ctrlPad_fromTopIO_calFinish, _T) @[ProcessingElement.scala 78:58]
    node _T_2 = eq(PEStateReg, UInt<2>("h2")) @[ProcessingElement.scala 81:34]
    node _T_3 = eq(UInt<2>("h0"), PEStateReg) @[Conditional.scala 37:30]
    node _GEN_0 = mux(io_ctrlTop_doLoadEn, UInt<2>("h1"), PEStateReg) @[ProcessingElement.scala 84:34]
    node _T_4 = eq(UInt<2>("h1"), PEStateReg) @[Conditional.scala 37:30]
    node _GEN_1 = mux(io_ctrlTop_writeFinish, UInt<2>("h2"), PEStateReg) @[ProcessingElement.scala 89:37]
    node _T_5 = eq(UInt<2>("h2"), PEStateReg) @[Conditional.scala 37:30]
    node _GEN_2 = mux(io_ctrlPad_fromTopIO_calFinish, UInt<2>("h0"), PEStateReg) @[ProcessingElement.scala 94:45]
    node _GEN_3 = mux(_T_5, _GEN_2, PEStateReg) @[Conditional.scala 39:67]
    node _GEN_4 = mux(_T_4, _GEN_1, _GEN_3) @[Conditional.scala 39:67]
    node _GEN_5 = mux(_T_3, _GEN_0, _GEN_4) @[Conditional.scala 40:58]
    io_ctrlPad_doMACEn <= _T_2 @[ProcessingElement.scala 81:22]
    io_ctrlPad_fromTopIO_pSumEnqEn <= io_ctrlTop_pSumEnqEn @[ProcessingElement.scala 79:34]
    io_ctrlPad_fromTopIO_doLoadEn <= io_ctrlTop_doLoadEn @[ProcessingElement.scala 80:33]
    io_ctrlTop_calFinish <= _T_1 @[ProcessingElement.scala 78:24]
    io_debugIO_peState <= PEStateReg @[ProcessingElement.scala 100:24]
    io_debugIO_doMACEnDebug <= io_ctrlPad_doMACEn @[ProcessingElement.scala 101:29]
    PEStateReg <= mux(reset, UInt<2>("h0"), _GEN_5) @[ProcessingElement.scala 85:18 ProcessingElement.scala 90:18 ProcessingElement.scala 95:18]

  module PSumSPad :
    input clock : Clock
    input reset : UInt<1>
    output io_dataPath_ipsIO_ready : UInt<1>
    input io_dataPath_ipsIO_valid : UInt<1>
    input io_dataPath_ipsIO_bits : UInt<21>
    input io_dataPath_opsIO_ready : UInt<1>
    output io_dataPath_opsIO_valid : UInt<1>
    output io_dataPath_opsIO_bits : UInt<21>
    input io_ctrlPath_readIdx : UInt<5>
    input io_ctrlPath_writeIdx : UInt<5>
  
    reg pSumDataSPadReg_0 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_0) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_1 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_1) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_2 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_2) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_3 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_3) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_4 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_4) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_5 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_5) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_6 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_6) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_7 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_7) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_8 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_8) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_9 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_9) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_10 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_10) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_11 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_11) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_12 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_12) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_13 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_13) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_14 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_14) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_15 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_15) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_16 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_16) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_17 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_17) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_18 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_18) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_19 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_19) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_20 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_20) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_21 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_21) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_22 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_22) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_23 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_23) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_24 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_24) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_25 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_25) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_26 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_26) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_27 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_27) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_28 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_28) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_29 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_29) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_30 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_30) @[SPadModule.scala 211:53]
    reg pSumDataSPadReg_31 : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumDataSPadReg_31) @[SPadModule.scala 211:53]
    node _GEN_0 = validif(eq(UInt<1>("h0"), io_ctrlPath_readIdx), pSumDataSPadReg_0) @[SPadModule.scala 214:19]
    node _GEN_1 = mux(eq(UInt<1>("h1"), io_ctrlPath_readIdx), pSumDataSPadReg_1, _GEN_0) @[SPadModule.scala 214:19]
    node _GEN_2 = mux(eq(UInt<2>("h2"), io_ctrlPath_readIdx), pSumDataSPadReg_2, _GEN_1) @[SPadModule.scala 214:19]
    node _GEN_3 = mux(eq(UInt<2>("h3"), io_ctrlPath_readIdx), pSumDataSPadReg_3, _GEN_2) @[SPadModule.scala 214:19]
    node _GEN_4 = mux(eq(UInt<3>("h4"), io_ctrlPath_readIdx), pSumDataSPadReg_4, _GEN_3) @[SPadModule.scala 214:19]
    node _GEN_5 = mux(eq(UInt<3>("h5"), io_ctrlPath_readIdx), pSumDataSPadReg_5, _GEN_4) @[SPadModule.scala 214:19]
    node _GEN_6 = mux(eq(UInt<3>("h6"), io_ctrlPath_readIdx), pSumDataSPadReg_6, _GEN_5) @[SPadModule.scala 214:19]
    node _GEN_7 = mux(eq(UInt<3>("h7"), io_ctrlPath_readIdx), pSumDataSPadReg_7, _GEN_6) @[SPadModule.scala 214:19]
    node _GEN_8 = mux(eq(UInt<4>("h8"), io_ctrlPath_readIdx), pSumDataSPadReg_8, _GEN_7) @[SPadModule.scala 214:19]
    node _GEN_9 = mux(eq(UInt<4>("h9"), io_ctrlPath_readIdx), pSumDataSPadReg_9, _GEN_8) @[SPadModule.scala 214:19]
    node _GEN_10 = mux(eq(UInt<4>("ha"), io_ctrlPath_readIdx), pSumDataSPadReg_10, _GEN_9) @[SPadModule.scala 214:19]
    node _GEN_11 = mux(eq(UInt<4>("hb"), io_ctrlPath_readIdx), pSumDataSPadReg_11, _GEN_10) @[SPadModule.scala 214:19]
    node _GEN_12 = mux(eq(UInt<4>("hc"), io_ctrlPath_readIdx), pSumDataSPadReg_12, _GEN_11) @[SPadModule.scala 214:19]
    node _GEN_13 = mux(eq(UInt<4>("hd"), io_ctrlPath_readIdx), pSumDataSPadReg_13, _GEN_12) @[SPadModule.scala 214:19]
    node _GEN_14 = mux(eq(UInt<4>("he"), io_ctrlPath_readIdx), pSumDataSPadReg_14, _GEN_13) @[SPadModule.scala 214:19]
    node _GEN_15 = mux(eq(UInt<4>("hf"), io_ctrlPath_readIdx), pSumDataSPadReg_15, _GEN_14) @[SPadModule.scala 214:19]
    node _GEN_16 = mux(eq(UInt<5>("h10"), io_ctrlPath_readIdx), pSumDataSPadReg_16, _GEN_15) @[SPadModule.scala 214:19]
    node _GEN_17 = mux(eq(UInt<5>("h11"), io_ctrlPath_readIdx), pSumDataSPadReg_17, _GEN_16) @[SPadModule.scala 214:19]
    node _GEN_18 = mux(eq(UInt<5>("h12"), io_ctrlPath_readIdx), pSumDataSPadReg_18, _GEN_17) @[SPadModule.scala 214:19]
    node _GEN_19 = mux(eq(UInt<5>("h13"), io_ctrlPath_readIdx), pSumDataSPadReg_19, _GEN_18) @[SPadModule.scala 214:19]
    node _GEN_20 = mux(eq(UInt<5>("h14"), io_ctrlPath_readIdx), pSumDataSPadReg_20, _GEN_19) @[SPadModule.scala 214:19]
    node _GEN_21 = mux(eq(UInt<5>("h15"), io_ctrlPath_readIdx), pSumDataSPadReg_21, _GEN_20) @[SPadModule.scala 214:19]
    node _GEN_22 = mux(eq(UInt<5>("h16"), io_ctrlPath_readIdx), pSumDataSPadReg_22, _GEN_21) @[SPadModule.scala 214:19]
    node _GEN_23 = mux(eq(UInt<5>("h17"), io_ctrlPath_readIdx), pSumDataSPadReg_23, _GEN_22) @[SPadModule.scala 214:19]
    node _GEN_24 = mux(eq(UInt<5>("h18"), io_ctrlPath_readIdx), pSumDataSPadReg_24, _GEN_23) @[SPadModule.scala 214:19]
    node _GEN_25 = mux(eq(UInt<5>("h19"), io_ctrlPath_readIdx), pSumDataSPadReg_25, _GEN_24) @[SPadModule.scala 214:19]
    node _GEN_26 = mux(eq(UInt<5>("h1a"), io_ctrlPath_readIdx), pSumDataSPadReg_26, _GEN_25) @[SPadModule.scala 214:19]
    node _GEN_27 = mux(eq(UInt<5>("h1b"), io_ctrlPath_readIdx), pSumDataSPadReg_27, _GEN_26) @[SPadModule.scala 214:19]
    node _GEN_28 = mux(eq(UInt<5>("h1c"), io_ctrlPath_readIdx), pSumDataSPadReg_28, _GEN_27) @[SPadModule.scala 214:19]
    node _GEN_29 = mux(eq(UInt<5>("h1d"), io_ctrlPath_readIdx), pSumDataSPadReg_29, _GEN_28) @[SPadModule.scala 214:19]
    node _GEN_30 = mux(eq(UInt<5>("h1e"), io_ctrlPath_readIdx), pSumDataSPadReg_30, _GEN_29) @[SPadModule.scala 214:19]
    node _GEN_31 = mux(eq(UInt<5>("h1f"), io_ctrlPath_readIdx), pSumDataSPadReg_31, _GEN_30) @[SPadModule.scala 214:19]
    node _T_1 = eq(io_dataPath_opsIO_ready, UInt<1>("h0")) @[SPadModule.scala 215:30]
    node _T_2 = eq(io_dataPath_ipsIO_valid, UInt<1>("h0")) @[SPadModule.scala 216:30]
    node _T_3 = and(io_dataPath_ipsIO_ready, io_dataPath_ipsIO_valid) @[Decoupled.scala 40:37]
    node _pSumDataSPadReg_io_ctrlPath_writeIdx = io_dataPath_ipsIO_bits @[SPadModule.scala 219:43 SPadModule.scala 219:43]
    node _GEN_32 = mux(eq(UInt<1>("h0"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_0) @[SPadModule.scala 219:43]
    node _GEN_33 = mux(eq(UInt<1>("h1"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_1) @[SPadModule.scala 219:43]
    node _GEN_34 = mux(eq(UInt<2>("h2"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_2) @[SPadModule.scala 219:43]
    node _GEN_35 = mux(eq(UInt<2>("h3"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_3) @[SPadModule.scala 219:43]
    node _GEN_36 = mux(eq(UInt<3>("h4"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_4) @[SPadModule.scala 219:43]
    node _GEN_37 = mux(eq(UInt<3>("h5"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_5) @[SPadModule.scala 219:43]
    node _GEN_38 = mux(eq(UInt<3>("h6"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_6) @[SPadModule.scala 219:43]
    node _GEN_39 = mux(eq(UInt<3>("h7"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_7) @[SPadModule.scala 219:43]
    node _GEN_40 = mux(eq(UInt<4>("h8"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_8) @[SPadModule.scala 219:43]
    node _GEN_41 = mux(eq(UInt<4>("h9"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_9) @[SPadModule.scala 219:43]
    node _GEN_42 = mux(eq(UInt<4>("ha"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_10) @[SPadModule.scala 219:43]
    node _GEN_43 = mux(eq(UInt<4>("hb"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_11) @[SPadModule.scala 219:43]
    node _GEN_44 = mux(eq(UInt<4>("hc"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_12) @[SPadModule.scala 219:43]
    node _GEN_45 = mux(eq(UInt<4>("hd"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_13) @[SPadModule.scala 219:43]
    node _GEN_46 = mux(eq(UInt<4>("he"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_14) @[SPadModule.scala 219:43]
    node _GEN_47 = mux(eq(UInt<4>("hf"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_15) @[SPadModule.scala 219:43]
    node _GEN_48 = mux(eq(UInt<5>("h10"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_16) @[SPadModule.scala 219:43]
    node _GEN_49 = mux(eq(UInt<5>("h11"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_17) @[SPadModule.scala 219:43]
    node _GEN_50 = mux(eq(UInt<5>("h12"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_18) @[SPadModule.scala 219:43]
    node _GEN_51 = mux(eq(UInt<5>("h13"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_19) @[SPadModule.scala 219:43]
    node _GEN_52 = mux(eq(UInt<5>("h14"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_20) @[SPadModule.scala 219:43]
    node _GEN_53 = mux(eq(UInt<5>("h15"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_21) @[SPadModule.scala 219:43]
    node _GEN_54 = mux(eq(UInt<5>("h16"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_22) @[SPadModule.scala 219:43]
    node _GEN_55 = mux(eq(UInt<5>("h17"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_23) @[SPadModule.scala 219:43]
    node _GEN_56 = mux(eq(UInt<5>("h18"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_24) @[SPadModule.scala 219:43]
    node _GEN_57 = mux(eq(UInt<5>("h19"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_25) @[SPadModule.scala 219:43]
    node _GEN_58 = mux(eq(UInt<5>("h1a"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_26) @[SPadModule.scala 219:43]
    node _GEN_59 = mux(eq(UInt<5>("h1b"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_27) @[SPadModule.scala 219:43]
    node _GEN_60 = mux(eq(UInt<5>("h1c"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_28) @[SPadModule.scala 219:43]
    node _GEN_61 = mux(eq(UInt<5>("h1d"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_29) @[SPadModule.scala 219:43]
    node _GEN_62 = mux(eq(UInt<5>("h1e"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_30) @[SPadModule.scala 219:43]
    node _GEN_63 = mux(eq(UInt<5>("h1f"), io_ctrlPath_writeIdx), _pSumDataSPadReg_io_ctrlPath_writeIdx, pSumDataSPadReg_31) @[SPadModule.scala 219:43]
    node _GEN_64 = mux(_T_3, _GEN_32, pSumDataSPadReg_0) @[SPadModule.scala 218:35]
    node _GEN_65 = mux(_T_3, _GEN_33, pSumDataSPadReg_1) @[SPadModule.scala 218:35]
    node _GEN_66 = mux(_T_3, _GEN_34, pSumDataSPadReg_2) @[SPadModule.scala 218:35]
    node _GEN_67 = mux(_T_3, _GEN_35, pSumDataSPadReg_3) @[SPadModule.scala 218:35]
    node _GEN_68 = mux(_T_3, _GEN_36, pSumDataSPadReg_4) @[SPadModule.scala 218:35]
    node _GEN_69 = mux(_T_3, _GEN_37, pSumDataSPadReg_5) @[SPadModule.scala 218:35]
    node _GEN_70 = mux(_T_3, _GEN_38, pSumDataSPadReg_6) @[SPadModule.scala 218:35]
    node _GEN_71 = mux(_T_3, _GEN_39, pSumDataSPadReg_7) @[SPadModule.scala 218:35]
    node _GEN_72 = mux(_T_3, _GEN_40, pSumDataSPadReg_8) @[SPadModule.scala 218:35]
    node _GEN_73 = mux(_T_3, _GEN_41, pSumDataSPadReg_9) @[SPadModule.scala 218:35]
    node _GEN_74 = mux(_T_3, _GEN_42, pSumDataSPadReg_10) @[SPadModule.scala 218:35]
    node _GEN_75 = mux(_T_3, _GEN_43, pSumDataSPadReg_11) @[SPadModule.scala 218:35]
    node _GEN_76 = mux(_T_3, _GEN_44, pSumDataSPadReg_12) @[SPadModule.scala 218:35]
    node _GEN_77 = mux(_T_3, _GEN_45, pSumDataSPadReg_13) @[SPadModule.scala 218:35]
    node _GEN_78 = mux(_T_3, _GEN_46, pSumDataSPadReg_14) @[SPadModule.scala 218:35]
    node _GEN_79 = mux(_T_3, _GEN_47, pSumDataSPadReg_15) @[SPadModule.scala 218:35]
    node _GEN_80 = mux(_T_3, _GEN_48, pSumDataSPadReg_16) @[SPadModule.scala 218:35]
    node _GEN_81 = mux(_T_3, _GEN_49, pSumDataSPadReg_17) @[SPadModule.scala 218:35]
    node _GEN_82 = mux(_T_3, _GEN_50, pSumDataSPadReg_18) @[SPadModule.scala 218:35]
    node _GEN_83 = mux(_T_3, _GEN_51, pSumDataSPadReg_19) @[SPadModule.scala 218:35]
    node _GEN_84 = mux(_T_3, _GEN_52, pSumDataSPadReg_20) @[SPadModule.scala 218:35]
    node _GEN_85 = mux(_T_3, _GEN_53, pSumDataSPadReg_21) @[SPadModule.scala 218:35]
    node _GEN_86 = mux(_T_3, _GEN_54, pSumDataSPadReg_22) @[SPadModule.scala 218:35]
    node _GEN_87 = mux(_T_3, _GEN_55, pSumDataSPadReg_23) @[SPadModule.scala 218:35]
    node _GEN_88 = mux(_T_3, _GEN_56, pSumDataSPadReg_24) @[SPadModule.scala 218:35]
    node _GEN_89 = mux(_T_3, _GEN_57, pSumDataSPadReg_25) @[SPadModule.scala 218:35]
    node _GEN_90 = mux(_T_3, _GEN_58, pSumDataSPadReg_26) @[SPadModule.scala 218:35]
    node _GEN_91 = mux(_T_3, _GEN_59, pSumDataSPadReg_27) @[SPadModule.scala 218:35]
    node _GEN_92 = mux(_T_3, _GEN_60, pSumDataSPadReg_28) @[SPadModule.scala 218:35]
    node _GEN_93 = mux(_T_3, _GEN_61, pSumDataSPadReg_29) @[SPadModule.scala 218:35]
    node _GEN_94 = mux(_T_3, _GEN_62, pSumDataSPadReg_30) @[SPadModule.scala 218:35]
    node _GEN_95 = mux(_T_3, _GEN_63, pSumDataSPadReg_31) @[SPadModule.scala 218:35]
    node _T__0 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__1 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__2 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__3 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__4 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__5 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__6 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__7 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__8 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__9 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__10 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__11 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__12 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__13 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__14 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__15 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__16 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__17 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__18 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__19 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__20 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__21 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__22 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__23 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__24 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__25 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__26 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__27 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__28 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__29 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__30 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _T__31 = UInt<21>("h0") @[SPadModule.scala 211:61 SPadModule.scala 211:61]
    node _pSumDataSPadReg_io_ctrlPath_readIdx = _GEN_31 @[SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19 SPadModule.scala 214:19]
    node readOutDataWire = _pSumDataSPadReg_io_ctrlPath_readIdx @[SPadModule.scala 213:39 SPadModule.scala 214:19]
    io_dataPath_ipsIO_ready <= _T_1 @[SPadModule.scala 215:27]
    io_dataPath_opsIO_valid <= _T_2 @[SPadModule.scala 216:27]
    io_dataPath_opsIO_bits <= readOutDataWire @[SPadModule.scala 217:26]
    pSumDataSPadReg_0 <= mux(reset, _T__0, _GEN_64) @[SPadModule.scala 219:43]
    pSumDataSPadReg_1 <= mux(reset, _T__1, _GEN_65) @[SPadModule.scala 219:43]
    pSumDataSPadReg_2 <= mux(reset, _T__2, _GEN_66) @[SPadModule.scala 219:43]
    pSumDataSPadReg_3 <= mux(reset, _T__3, _GEN_67) @[SPadModule.scala 219:43]
    pSumDataSPadReg_4 <= mux(reset, _T__4, _GEN_68) @[SPadModule.scala 219:43]
    pSumDataSPadReg_5 <= mux(reset, _T__5, _GEN_69) @[SPadModule.scala 219:43]
    pSumDataSPadReg_6 <= mux(reset, _T__6, _GEN_70) @[SPadModule.scala 219:43]
    pSumDataSPadReg_7 <= mux(reset, _T__7, _GEN_71) @[SPadModule.scala 219:43]
    pSumDataSPadReg_8 <= mux(reset, _T__8, _GEN_72) @[SPadModule.scala 219:43]
    pSumDataSPadReg_9 <= mux(reset, _T__9, _GEN_73) @[SPadModule.scala 219:43]
    pSumDataSPadReg_10 <= mux(reset, _T__10, _GEN_74) @[SPadModule.scala 219:43]
    pSumDataSPadReg_11 <= mux(reset, _T__11, _GEN_75) @[SPadModule.scala 219:43]
    pSumDataSPadReg_12 <= mux(reset, _T__12, _GEN_76) @[SPadModule.scala 219:43]
    pSumDataSPadReg_13 <= mux(reset, _T__13, _GEN_77) @[SPadModule.scala 219:43]
    pSumDataSPadReg_14 <= mux(reset, _T__14, _GEN_78) @[SPadModule.scala 219:43]
    pSumDataSPadReg_15 <= mux(reset, _T__15, _GEN_79) @[SPadModule.scala 219:43]
    pSumDataSPadReg_16 <= mux(reset, _T__16, _GEN_80) @[SPadModule.scala 219:43]
    pSumDataSPadReg_17 <= mux(reset, _T__17, _GEN_81) @[SPadModule.scala 219:43]
    pSumDataSPadReg_18 <= mux(reset, _T__18, _GEN_82) @[SPadModule.scala 219:43]
    pSumDataSPadReg_19 <= mux(reset, _T__19, _GEN_83) @[SPadModule.scala 219:43]
    pSumDataSPadReg_20 <= mux(reset, _T__20, _GEN_84) @[SPadModule.scala 219:43]
    pSumDataSPadReg_21 <= mux(reset, _T__21, _GEN_85) @[SPadModule.scala 219:43]
    pSumDataSPadReg_22 <= mux(reset, _T__22, _GEN_86) @[SPadModule.scala 219:43]
    pSumDataSPadReg_23 <= mux(reset, _T__23, _GEN_87) @[SPadModule.scala 219:43]
    pSumDataSPadReg_24 <= mux(reset, _T__24, _GEN_88) @[SPadModule.scala 219:43]
    pSumDataSPadReg_25 <= mux(reset, _T__25, _GEN_89) @[SPadModule.scala 219:43]
    pSumDataSPadReg_26 <= mux(reset, _T__26, _GEN_90) @[SPadModule.scala 219:43]
    pSumDataSPadReg_27 <= mux(reset, _T__27, _GEN_91) @[SPadModule.scala 219:43]
    pSumDataSPadReg_28 <= mux(reset, _T__28, _GEN_92) @[SPadModule.scala 219:43]
    pSumDataSPadReg_29 <= mux(reset, _T__29, _GEN_93) @[SPadModule.scala 219:43]
    pSumDataSPadReg_30 <= mux(reset, _T__30, _GEN_94) @[SPadModule.scala 219:43]
    pSumDataSPadReg_31 <= mux(reset, _T__31, _GEN_95) @[SPadModule.scala 219:43]

  module SPadDataModule :
    input clock : Clock
    input reset : UInt<1>
    output io_dataPath_columnNum : UInt<4>
    output io_dataPath_readOutData : UInt<7>
    output io_dataPath_writeInData_data_ready : UInt<1>
    input io_dataPath_writeInData_data_valid : UInt<1>
    input io_dataPath_writeInData_data_bits : UInt<7>
    input io_ctrlPath_writeEn : UInt<1>
    output io_ctrlPath_writeIdx : UInt<4>
    output io_ctrlPath_writeFin : UInt<1>
    input io_ctrlPath_readEn : UInt<1>
    input io_ctrlPath_readInIdx : UInt<4>
    input io_ctrlPath_indexInc : UInt<1>
    input io_ctrlPath_readInIdxEn : UInt<1>
  
    reg padWriteIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padWriteIndexReg) @[SPadModule.scala 183:49]
    reg padReadIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padReadIndexReg) @[SPadModule.scala 184:48]
    node _T = and(io_dataPath_writeInData_data_ready, io_dataPath_writeInData_data_valid) @[Decoupled.scala 40:37]
    node _T_1 = and(_T, io_ctrlPath_writeEn) @[SPadModule.scala 189:39]
    node _T_2 = eq(io_dataPath_writeInData_data_bits, UInt<1>("h0")) @[SPadModule.scala 190:41]
    node writeFire = _T_1 @[SPadModule.scala 188:39 SPadModule.scala 189:13]
    node _T_3 = and(_T_2, writeFire) @[SPadModule.scala 190:49]
    reg dataSPadReg_8 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_8) @[SPadModule.scala 165:38]
    reg dataSPadReg_7 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_7) @[SPadModule.scala 165:38]
    reg dataSPadReg_6 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_6) @[SPadModule.scala 165:38]
    reg dataSPadReg_5 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_5) @[SPadModule.scala 165:38]
    reg dataSPadReg_4 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_4) @[SPadModule.scala 165:38]
    reg dataSPadReg_3 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_3) @[SPadModule.scala 165:38]
    reg dataSPadReg_2 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_2) @[SPadModule.scala 165:38]
    reg dataSPadReg_1 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_1) @[SPadModule.scala 165:38]
    reg dataSPadReg_0 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_0) @[SPadModule.scala 165:38]
    node _GEN_20 = validif(eq(UInt<1>("h0"), padReadIndexReg), dataSPadReg_0) @[SPadModule.scala 173:14]
    node _GEN_21 = mux(eq(UInt<1>("h1"), padReadIndexReg), dataSPadReg_1, _GEN_20) @[SPadModule.scala 173:14]
    node _GEN_22 = mux(eq(UInt<2>("h2"), padReadIndexReg), dataSPadReg_2, _GEN_21) @[SPadModule.scala 173:14]
    node _GEN_23 = mux(eq(UInt<2>("h3"), padReadIndexReg), dataSPadReg_3, _GEN_22) @[SPadModule.scala 173:14]
    node _GEN_24 = mux(eq(UInt<3>("h4"), padReadIndexReg), dataSPadReg_4, _GEN_23) @[SPadModule.scala 173:14]
    node _GEN_25 = mux(eq(UInt<3>("h5"), padReadIndexReg), dataSPadReg_5, _GEN_24) @[SPadModule.scala 173:14]
    node _GEN_26 = mux(eq(UInt<3>("h6"), padReadIndexReg), dataSPadReg_6, _GEN_25) @[SPadModule.scala 173:14]
    node _GEN_27 = mux(eq(UInt<3>("h7"), padReadIndexReg), dataSPadReg_7, _GEN_26) @[SPadModule.scala 173:14]
    node _GEN_28 = mux(eq(UInt<4>("h8"), padReadIndexReg), dataSPadReg_8, _GEN_27) @[SPadModule.scala 173:14]
    node _dataSPadReg_padReadIndexReg = _GEN_28 @[SPadModule.scala 173:14 SPadModule.scala 173:14 SPadModule.scala 173:14 SPadModule.scala 173:14 SPadModule.scala 173:14 SPadModule.scala 173:14 SPadModule.scala 173:14 SPadModule.scala 173:14 SPadModule.scala 173:14 SPadModule.scala 173:14]
    node dataWire = _dataSPadReg_padReadIndexReg @[SPadModule.scala 182:38 SPadModule.scala 173:14]
    node _T_4 = eq(dataWire, UInt<1>("h0")) @[SPadModule.scala 191:28]
    node readIndexInc = io_ctrlPath_indexInc @[SPadModule.scala 187:42 SPadModule.scala 175:16]
    node _T_5 = and(_T_4, readIndexInc) @[SPadModule.scala 191:36]
    node _T_6 = add(padWriteIndexReg, UInt<1>("h1")) @[SPadModule.scala 194:79]
    node _T_7 = tail(_T_6, 1) @[SPadModule.scala 194:79]
    node writeWrapWire = _T_3 @[SPadModule.scala 185:43 SPadModule.scala 190:17]
    node _T_8 = mux(writeWrapWire, UInt<1>("h0"), _T_7) @[SPadModule.scala 194:41]
    node _T_9 = mux(writeFire, _T_8, padWriteIndexReg) @[SPadModule.scala 194:26]
    node _dataSPadReg_padWriteIndexReg = io_dataPath_writeInData_data_bits @[SPadModule.scala 168:34 SPadModule.scala 168:34]
    node _GEN_0 = mux(eq(UInt<1>("h0"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_0) @[SPadModule.scala 168:34]
    node _GEN_1 = mux(eq(UInt<1>("h1"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_1) @[SPadModule.scala 168:34]
    node _GEN_2 = mux(eq(UInt<2>("h2"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_2) @[SPadModule.scala 168:34]
    node _GEN_3 = mux(eq(UInt<2>("h3"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_3) @[SPadModule.scala 168:34]
    node _GEN_4 = mux(eq(UInt<3>("h4"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_4) @[SPadModule.scala 168:34]
    node _GEN_5 = mux(eq(UInt<3>("h5"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_5) @[SPadModule.scala 168:34]
    node _GEN_6 = mux(eq(UInt<3>("h6"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_6) @[SPadModule.scala 168:34]
    node _GEN_7 = mux(eq(UInt<3>("h7"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_7) @[SPadModule.scala 168:34]
    node _GEN_8 = mux(eq(UInt<4>("h8"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_8) @[SPadModule.scala 168:34]
    node _GEN_9 = mux(writeFire, _GEN_0, dataSPadReg_0) @[SPadModule.scala 167:22]
    node _GEN_10 = mux(writeFire, _GEN_1, dataSPadReg_1) @[SPadModule.scala 167:22]
    node _GEN_11 = mux(writeFire, _GEN_2, dataSPadReg_2) @[SPadModule.scala 167:22]
    node _GEN_12 = mux(writeFire, _GEN_3, dataSPadReg_3) @[SPadModule.scala 167:22]
    node _GEN_13 = mux(writeFire, _GEN_4, dataSPadReg_4) @[SPadModule.scala 167:22]
    node _GEN_14 = mux(writeFire, _GEN_5, dataSPadReg_5) @[SPadModule.scala 167:22]
    node _GEN_15 = mux(writeFire, _GEN_6, dataSPadReg_6) @[SPadModule.scala 167:22]
    node _GEN_16 = mux(writeFire, _GEN_7, dataSPadReg_7) @[SPadModule.scala 167:22]
    node _GEN_17 = mux(writeFire, _GEN_8, dataSPadReg_8) @[SPadModule.scala 167:22]
    node _T_11 = add(padReadIndexReg, UInt<1>("h1")) @[SPadModule.scala 201:42]
    node _T_12 = tail(_T_11, 1) @[SPadModule.scala 201:42]
    node readWrapWire = _T_5 @[SPadModule.scala 186:42 SPadModule.scala 191:16]
    node _GEN_18 = mux(readWrapWire, UInt<1>("h0"), _T_12) @[SPadModule.scala 202:27]
    node _GEN_19 = mux(readIndexInc, _GEN_18, padReadIndexReg) @[SPadModule.scala 200:25]
    node _T_10_0 = UInt<7>("h0") @[SPadModule.scala 165:46 SPadModule.scala 165:46]
    node _T_10_1 = UInt<7>("h0") @[SPadModule.scala 165:46 SPadModule.scala 165:46]
    node _T_10_2 = UInt<7>("h0") @[SPadModule.scala 165:46 SPadModule.scala 165:46]
    node _T_10_3 = UInt<7>("h0") @[SPadModule.scala 165:46 SPadModule.scala 165:46]
    node _T_10_4 = UInt<7>("h0") @[SPadModule.scala 165:46 SPadModule.scala 165:46]
    node _T_10_5 = UInt<7>("h0") @[SPadModule.scala 165:46 SPadModule.scala 165:46]
    node _T_10_6 = UInt<7>("h0") @[SPadModule.scala 165:46 SPadModule.scala 165:46]
    node _T_10_7 = UInt<7>("h0") @[SPadModule.scala 165:46 SPadModule.scala 165:46]
    node _T_10_8 = UInt<7>("h0") @[SPadModule.scala 165:46 SPadModule.scala 165:46]
    io_dataPath_columnNum <= padReadIndexReg @[SPadModule.scala 198:25]
    io_dataPath_readOutData <= dataWire @[SPadModule.scala 176:27]
    io_dataPath_writeInData_data_ready <= UInt<1>("h1") @[SPadModule.scala 193:25]
    io_ctrlPath_writeIdx <= padWriteIndexReg @[SPadModule.scala 196:24]
    io_ctrlPath_writeFin <= writeWrapWire @[SPadModule.scala 197:24]
    padWriteIndexReg <= mux(reset, UInt<4>("h0"), _T_9) @[SPadModule.scala 194:20]
    padReadIndexReg <= mux(reset, UInt<4>("h0"), _GEN_19) @[SPadModule.scala 201:23 SPadModule.scala 203:25]
    dataSPadReg_0 <= mux(reset, _T_10_0, _GEN_9) @[SPadModule.scala 168:34]
    dataSPadReg_1 <= mux(reset, _T_10_1, _GEN_10) @[SPadModule.scala 168:34]
    dataSPadReg_2 <= mux(reset, _T_10_2, _GEN_11) @[SPadModule.scala 168:34]
    dataSPadReg_3 <= mux(reset, _T_10_3, _GEN_12) @[SPadModule.scala 168:34]
    dataSPadReg_4 <= mux(reset, _T_10_4, _GEN_13) @[SPadModule.scala 168:34]
    dataSPadReg_5 <= mux(reset, _T_10_5, _GEN_14) @[SPadModule.scala 168:34]
    dataSPadReg_6 <= mux(reset, _T_10_6, _GEN_15) @[SPadModule.scala 168:34]
    dataSPadReg_7 <= mux(reset, _T_10_7, _GEN_16) @[SPadModule.scala 168:34]
    dataSPadReg_8 <= mux(reset, _T_10_8, _GEN_17) @[SPadModule.scala 168:34]

  module nonTpActSPadDataModule :
    input clock : Clock
    input reset : UInt<1>
    output io_dataPath_columnNum : UInt<6>
    output io_dataPath_readOutData : UInt<12>
    output io_dataPath_writeInData_data_ready : UInt<1>
    input io_dataPath_writeInData_data_valid : UInt<1>
    input io_dataPath_writeInData_data_bits : UInt<12>
    input io_dataPath_currentSliding : UInt<1>
    output io_dataPath_slidingBoxUB : UInt<4>
    output io_dataPath_slidingBoxLB : UInt<4>
    output io_dataPath_futureLBStart : UInt<4>
    input io_ctrlPath_writeEn : UInt<1>
    output io_ctrlPath_writeIdx : UInt<6>
    output io_ctrlPath_writeFin : UInt<1>
    input io_ctrlPath_readEn : UInt<1>
    input io_ctrlPath_readInIdx : UInt<6>
    input io_ctrlPath_indexInc : UInt<1>
    input io_ctrlPath_readInIdxEn : UInt<1>
    output io_ctrlPath_slidingInc : UInt<1>
    input io_ctrlPath_inActColInc : UInt<1>
    input io_ctrlPath_weightIdxInc : UInt<1>
    input io_ctrlPath_padEqMpyBool : UInt<1>
    input io_ctrlPath_mightWeightZeroColumn : UInt<1>
    input io_ctrlPath_padEqWABool : UInt<1>
    input io_ctrlPath_padEqIDBool : UInt<1>
    output io_ctrlPath_inActReadIdxBeyondUB : UInt<1>
    output io_ctrlPath_inActLastNonZeroEle : UInt<1>
  
    reg padWriteIndexReg : UInt<6>, clock with :
      reset => (UInt<1>("h0"), padWriteIndexReg) @[SPadModule.scala 11:49]
    reg padReadIndexReg : UInt<6>, clock with :
      reset => (UInt<1>("h0"), padReadIndexReg) @[SPadModule.scala 12:48]
    reg readSlidingInc : UInt<1>, clock with :
      reset => (UInt<1>("h0"), readSlidingInc) @[SPadModule.scala 18:47]
    reg inActDataLB : UInt<4>, clock with :
      reset => (UInt<1>("h0"), inActDataLB) @[SPadModule.scala 20:44]
    reg inActDataUB : UInt<4>, clock with :
      reset => (UInt<1>("h0"), inActDataUB) @[SPadModule.scala 21:44]
    reg futureInActDataLB : UInt<4>, clock with :
      reset => (UInt<1>("h0"), futureInActDataLB) @[SPadModule.scala 22:50]
    reg dataSPadReg_0 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_0) @[SPadModule.scala 23:46]
    reg dataSPadReg_1 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_1) @[SPadModule.scala 23:46]
    reg dataSPadReg_2 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_2) @[SPadModule.scala 23:46]
    reg dataSPadReg_3 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_3) @[SPadModule.scala 23:46]
    reg dataSPadReg_4 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_4) @[SPadModule.scala 23:46]
    reg dataSPadReg_5 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_5) @[SPadModule.scala 23:46]
    reg dataSPadReg_6 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_6) @[SPadModule.scala 23:46]
    reg dataSPadReg_7 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_7) @[SPadModule.scala 23:46]
    reg dataSPadReg_8 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_8) @[SPadModule.scala 23:46]
    reg dataSPadReg_9 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_9) @[SPadModule.scala 23:46]
    reg dataSPadReg_10 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_10) @[SPadModule.scala 23:46]
    reg dataSPadReg_11 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_11) @[SPadModule.scala 23:46]
    reg dataSPadReg_12 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_12) @[SPadModule.scala 23:46]
    reg dataSPadReg_13 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_13) @[SPadModule.scala 23:46]
    reg dataSPadReg_14 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_14) @[SPadModule.scala 23:46]
    reg dataSPadReg_15 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_15) @[SPadModule.scala 23:46]
    reg dataSPadReg_16 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_16) @[SPadModule.scala 23:46]
    reg dataSPadReg_17 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_17) @[SPadModule.scala 23:46]
    reg dataSPadReg_18 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_18) @[SPadModule.scala 23:46]
    reg dataSPadReg_19 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_19) @[SPadModule.scala 23:46]
    reg dataSPadReg_20 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_20) @[SPadModule.scala 23:46]
    reg dataSPadReg_21 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_21) @[SPadModule.scala 23:46]
    reg dataSPadReg_22 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_22) @[SPadModule.scala 23:46]
    reg dataSPadReg_23 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_23) @[SPadModule.scala 23:46]
    reg dataSPadReg_24 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_24) @[SPadModule.scala 23:46]
    reg dataSPadReg_25 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_25) @[SPadModule.scala 23:46]
    reg dataSPadReg_26 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_26) @[SPadModule.scala 23:46]
    reg dataSPadReg_27 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_27) @[SPadModule.scala 23:46]
    reg dataSPadReg_28 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_28) @[SPadModule.scala 23:46]
    reg dataSPadReg_29 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_29) @[SPadModule.scala 23:46]
    reg dataSPadReg_30 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_30) @[SPadModule.scala 23:46]
    reg dataSPadReg_31 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_31) @[SPadModule.scala 23:46]
    reg dataSPadReg_32 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_32) @[SPadModule.scala 23:46]
    reg dataSPadReg_33 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_33) @[SPadModule.scala 23:46]
    reg dataSPadReg_34 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_34) @[SPadModule.scala 23:46]
    reg dataSPadReg_35 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_35) @[SPadModule.scala 23:46]
    reg dataSPadReg_36 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_36) @[SPadModule.scala 23:46]
    reg dataSPadReg_37 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_37) @[SPadModule.scala 23:46]
    reg dataSPadReg_38 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_38) @[SPadModule.scala 23:46]
    reg dataSPadReg_39 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_39) @[SPadModule.scala 23:46]
    reg dataSPadReg_40 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_40) @[SPadModule.scala 23:46]
    reg dataSPadReg_41 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_41) @[SPadModule.scala 23:46]
    reg dataSPadReg_42 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_42) @[SPadModule.scala 23:46]
    reg dataSPadReg_43 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_43) @[SPadModule.scala 23:46]
    reg dataSPadReg_44 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_44) @[SPadModule.scala 23:46]
    reg dataSPadReg_45 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_45) @[SPadModule.scala 23:46]
    reg dataSPadReg_46 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_46) @[SPadModule.scala 23:46]
    reg dataSPadReg_47 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_47) @[SPadModule.scala 23:46]
    reg dataSPadReg_48 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_48) @[SPadModule.scala 23:46]
    reg dataSPadReg_49 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_49) @[SPadModule.scala 23:46]
    reg dataSPadReg_50 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_50) @[SPadModule.scala 23:46]
    reg dataSPadReg_51 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_51) @[SPadModule.scala 23:46]
    reg dataSPadReg_52 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_52) @[SPadModule.scala 23:46]
    reg dataSPadReg_53 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_53) @[SPadModule.scala 23:46]
    reg dataSPadReg_54 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_54) @[SPadModule.scala 23:46]
    reg dataSPadReg_55 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_55) @[SPadModule.scala 23:46]
    reg dataSPadReg_56 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_56) @[SPadModule.scala 23:46]
    reg dataSPadReg_57 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_57) @[SPadModule.scala 23:46]
    reg dataSPadReg_58 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_58) @[SPadModule.scala 23:46]
    reg dataSPadReg_59 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_59) @[SPadModule.scala 23:46]
    reg dataSPadReg_60 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_60) @[SPadModule.scala 23:46]
    reg dataSPadReg_61 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_61) @[SPadModule.scala 23:46]
    reg dataSPadReg_62 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_62) @[SPadModule.scala 23:46]
    reg dataSPadReg_63 : UInt<12>, clock with :
      reset => (UInt<1>("h0"), dataSPadReg_63) @[SPadModule.scala 23:46]
    reg currentColTopIdxReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), currentColTopIdxReg) @[SPadModule.scala 25:52]
    reg futureLBStartReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), futureLBStartReg) @[SPadModule.scala 27:49]
    reg currentCountAtFutureLB : UInt<1>, clock with :
      reset => (UInt<1>("h0"), currentCountAtFutureLB) @[SPadModule.scala 33:55]
    reg lastCountNotAtFutureLB : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lastCountNotAtFutureLB) @[SPadModule.scala 34:55]
    reg lastInActAdrDataReg : UInt<7>, clock with :
      reset => (UInt<1>("h0"), lastInActAdrDataReg) @[SPadModule.scala 37:52]
    node _T_1 = and(io_dataPath_writeInData_data_ready, io_dataPath_writeInData_data_valid) @[Decoupled.scala 40:37]
    node _T_2 = and(_T_1, io_ctrlPath_writeEn) @[SPadModule.scala 43:39]
    node _T_3 = eq(io_dataPath_writeInData_data_bits, UInt<1>("h0")) @[SPadModule.scala 44:41]
    node writeFire = _T_2 @[SPadModule.scala 16:39 SPadModule.scala 43:13]
    node _T_4 = and(_T_3, writeFire) @[SPadModule.scala 44:49]
    node _GEN_0 = validif(eq(UInt<1>("h0"), padReadIndexReg), dataSPadReg_0) @[SPadModule.scala 116:30]
    node _GEN_1 = mux(eq(UInt<1>("h1"), padReadIndexReg), dataSPadReg_1, _GEN_0) @[SPadModule.scala 116:30]
    node _GEN_2 = mux(eq(UInt<2>("h2"), padReadIndexReg), dataSPadReg_2, _GEN_1) @[SPadModule.scala 116:30]
    node _GEN_3 = mux(eq(UInt<2>("h3"), padReadIndexReg), dataSPadReg_3, _GEN_2) @[SPadModule.scala 116:30]
    node _GEN_4 = mux(eq(UInt<3>("h4"), padReadIndexReg), dataSPadReg_4, _GEN_3) @[SPadModule.scala 116:30]
    node _GEN_5 = mux(eq(UInt<3>("h5"), padReadIndexReg), dataSPadReg_5, _GEN_4) @[SPadModule.scala 116:30]
    node _GEN_6 = mux(eq(UInt<3>("h6"), padReadIndexReg), dataSPadReg_6, _GEN_5) @[SPadModule.scala 116:30]
    node _GEN_7 = mux(eq(UInt<3>("h7"), padReadIndexReg), dataSPadReg_7, _GEN_6) @[SPadModule.scala 116:30]
    node _GEN_8 = mux(eq(UInt<4>("h8"), padReadIndexReg), dataSPadReg_8, _GEN_7) @[SPadModule.scala 116:30]
    node _GEN_9 = mux(eq(UInt<4>("h9"), padReadIndexReg), dataSPadReg_9, _GEN_8) @[SPadModule.scala 116:30]
    node _GEN_10 = mux(eq(UInt<4>("ha"), padReadIndexReg), dataSPadReg_10, _GEN_9) @[SPadModule.scala 116:30]
    node _GEN_11 = mux(eq(UInt<4>("hb"), padReadIndexReg), dataSPadReg_11, _GEN_10) @[SPadModule.scala 116:30]
    node _GEN_12 = mux(eq(UInt<4>("hc"), padReadIndexReg), dataSPadReg_12, _GEN_11) @[SPadModule.scala 116:30]
    node _GEN_13 = mux(eq(UInt<4>("hd"), padReadIndexReg), dataSPadReg_13, _GEN_12) @[SPadModule.scala 116:30]
    node _GEN_14 = mux(eq(UInt<4>("he"), padReadIndexReg), dataSPadReg_14, _GEN_13) @[SPadModule.scala 116:30]
    node _GEN_15 = mux(eq(UInt<4>("hf"), padReadIndexReg), dataSPadReg_15, _GEN_14) @[SPadModule.scala 116:30]
    node _GEN_16 = mux(eq(UInt<5>("h10"), padReadIndexReg), dataSPadReg_16, _GEN_15) @[SPadModule.scala 116:30]
    node _GEN_17 = mux(eq(UInt<5>("h11"), padReadIndexReg), dataSPadReg_17, _GEN_16) @[SPadModule.scala 116:30]
    node _GEN_18 = mux(eq(UInt<5>("h12"), padReadIndexReg), dataSPadReg_18, _GEN_17) @[SPadModule.scala 116:30]
    node _GEN_19 = mux(eq(UInt<5>("h13"), padReadIndexReg), dataSPadReg_19, _GEN_18) @[SPadModule.scala 116:30]
    node _GEN_20 = mux(eq(UInt<5>("h14"), padReadIndexReg), dataSPadReg_20, _GEN_19) @[SPadModule.scala 116:30]
    node _GEN_21 = mux(eq(UInt<5>("h15"), padReadIndexReg), dataSPadReg_21, _GEN_20) @[SPadModule.scala 116:30]
    node _GEN_22 = mux(eq(UInt<5>("h16"), padReadIndexReg), dataSPadReg_22, _GEN_21) @[SPadModule.scala 116:30]
    node _GEN_23 = mux(eq(UInt<5>("h17"), padReadIndexReg), dataSPadReg_23, _GEN_22) @[SPadModule.scala 116:30]
    node _GEN_24 = mux(eq(UInt<5>("h18"), padReadIndexReg), dataSPadReg_24, _GEN_23) @[SPadModule.scala 116:30]
    node _GEN_25 = mux(eq(UInt<5>("h19"), padReadIndexReg), dataSPadReg_25, _GEN_24) @[SPadModule.scala 116:30]
    node _GEN_26 = mux(eq(UInt<5>("h1a"), padReadIndexReg), dataSPadReg_26, _GEN_25) @[SPadModule.scala 116:30]
    node _GEN_27 = mux(eq(UInt<5>("h1b"), padReadIndexReg), dataSPadReg_27, _GEN_26) @[SPadModule.scala 116:30]
    node _GEN_28 = mux(eq(UInt<5>("h1c"), padReadIndexReg), dataSPadReg_28, _GEN_27) @[SPadModule.scala 116:30]
    node _GEN_29 = mux(eq(UInt<5>("h1d"), padReadIndexReg), dataSPadReg_29, _GEN_28) @[SPadModule.scala 116:30]
    node _GEN_30 = mux(eq(UInt<5>("h1e"), padReadIndexReg), dataSPadReg_30, _GEN_29) @[SPadModule.scala 116:30]
    node _GEN_31 = mux(eq(UInt<5>("h1f"), padReadIndexReg), dataSPadReg_31, _GEN_30) @[SPadModule.scala 116:30]
    node _GEN_32 = mux(eq(UInt<6>("h20"), padReadIndexReg), dataSPadReg_32, _GEN_31) @[SPadModule.scala 116:30]
    node _GEN_33 = mux(eq(UInt<6>("h21"), padReadIndexReg), dataSPadReg_33, _GEN_32) @[SPadModule.scala 116:30]
    node _GEN_34 = mux(eq(UInt<6>("h22"), padReadIndexReg), dataSPadReg_34, _GEN_33) @[SPadModule.scala 116:30]
    node _GEN_35 = mux(eq(UInt<6>("h23"), padReadIndexReg), dataSPadReg_35, _GEN_34) @[SPadModule.scala 116:30]
    node _GEN_36 = mux(eq(UInt<6>("h24"), padReadIndexReg), dataSPadReg_36, _GEN_35) @[SPadModule.scala 116:30]
    node _GEN_37 = mux(eq(UInt<6>("h25"), padReadIndexReg), dataSPadReg_37, _GEN_36) @[SPadModule.scala 116:30]
    node _GEN_38 = mux(eq(UInt<6>("h26"), padReadIndexReg), dataSPadReg_38, _GEN_37) @[SPadModule.scala 116:30]
    node _GEN_39 = mux(eq(UInt<6>("h27"), padReadIndexReg), dataSPadReg_39, _GEN_38) @[SPadModule.scala 116:30]
    node _GEN_40 = mux(eq(UInt<6>("h28"), padReadIndexReg), dataSPadReg_40, _GEN_39) @[SPadModule.scala 116:30]
    node _GEN_41 = mux(eq(UInt<6>("h29"), padReadIndexReg), dataSPadReg_41, _GEN_40) @[SPadModule.scala 116:30]
    node _GEN_42 = mux(eq(UInt<6>("h2a"), padReadIndexReg), dataSPadReg_42, _GEN_41) @[SPadModule.scala 116:30]
    node _GEN_43 = mux(eq(UInt<6>("h2b"), padReadIndexReg), dataSPadReg_43, _GEN_42) @[SPadModule.scala 116:30]
    node _GEN_44 = mux(eq(UInt<6>("h2c"), padReadIndexReg), dataSPadReg_44, _GEN_43) @[SPadModule.scala 116:30]
    node _GEN_45 = mux(eq(UInt<6>("h2d"), padReadIndexReg), dataSPadReg_45, _GEN_44) @[SPadModule.scala 116:30]
    node _GEN_46 = mux(eq(UInt<6>("h2e"), padReadIndexReg), dataSPadReg_46, _GEN_45) @[SPadModule.scala 116:30]
    node _GEN_47 = mux(eq(UInt<6>("h2f"), padReadIndexReg), dataSPadReg_47, _GEN_46) @[SPadModule.scala 116:30]
    node _GEN_48 = mux(eq(UInt<6>("h30"), padReadIndexReg), dataSPadReg_48, _GEN_47) @[SPadModule.scala 116:30]
    node _GEN_49 = mux(eq(UInt<6>("h31"), padReadIndexReg), dataSPadReg_49, _GEN_48) @[SPadModule.scala 116:30]
    node _GEN_50 = mux(eq(UInt<6>("h32"), padReadIndexReg), dataSPadReg_50, _GEN_49) @[SPadModule.scala 116:30]
    node _GEN_51 = mux(eq(UInt<6>("h33"), padReadIndexReg), dataSPadReg_51, _GEN_50) @[SPadModule.scala 116:30]
    node _GEN_52 = mux(eq(UInt<6>("h34"), padReadIndexReg), dataSPadReg_52, _GEN_51) @[SPadModule.scala 116:30]
    node _GEN_53 = mux(eq(UInt<6>("h35"), padReadIndexReg), dataSPadReg_53, _GEN_52) @[SPadModule.scala 116:30]
    node _GEN_54 = mux(eq(UInt<6>("h36"), padReadIndexReg), dataSPadReg_54, _GEN_53) @[SPadModule.scala 116:30]
    node _GEN_55 = mux(eq(UInt<6>("h37"), padReadIndexReg), dataSPadReg_55, _GEN_54) @[SPadModule.scala 116:30]
    node _GEN_56 = mux(eq(UInt<6>("h38"), padReadIndexReg), dataSPadReg_56, _GEN_55) @[SPadModule.scala 116:30]
    node _GEN_57 = mux(eq(UInt<6>("h39"), padReadIndexReg), dataSPadReg_57, _GEN_56) @[SPadModule.scala 116:30]
    node _GEN_58 = mux(eq(UInt<6>("h3a"), padReadIndexReg), dataSPadReg_58, _GEN_57) @[SPadModule.scala 116:30]
    node _GEN_59 = mux(eq(UInt<6>("h3b"), padReadIndexReg), dataSPadReg_59, _GEN_58) @[SPadModule.scala 116:30]
    node _GEN_60 = mux(eq(UInt<6>("h3c"), padReadIndexReg), dataSPadReg_60, _GEN_59) @[SPadModule.scala 116:30]
    node _GEN_61 = mux(eq(UInt<6>("h3d"), padReadIndexReg), dataSPadReg_61, _GEN_60) @[SPadModule.scala 116:30]
    node _GEN_62 = mux(eq(UInt<6>("h3e"), padReadIndexReg), dataSPadReg_62, _GEN_61) @[SPadModule.scala 116:30]
    node _GEN_63 = mux(eq(UInt<6>("h3f"), padReadIndexReg), dataSPadReg_63, _GEN_62) @[SPadModule.scala 116:30]
    node _dataSPadReg_padReadIndexReg_1 = _GEN_63 @[SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12 SPadModule.scala 114:12]
    node dataWire = _dataSPadReg_padReadIndexReg_1 @[SPadModule.scala 10:38 SPadModule.scala 114:12]
    node _T_5 = eq(dataWire, UInt<1>("h0")) @[SPadModule.scala 45:29]
    node _T_6 = div(UInt<2>("h2"), UInt<1>("h1")) @[SPadModule.scala 45:70]
    node _T_7 = sub(_T_6, UInt<1>("h1")) @[SPadModule.scala 45:76]
    node _T_8 = tail(_T_7, 1) @[SPadModule.scala 45:76]
    node currentSlidingWire = io_dataPath_currentSliding @[SPadModule.scala 19:48 SPadModule.scala 60:22]
    node _T_9 = eq(currentSlidingWire, _T_8) @[SPadModule.scala 45:61]
    node _T_10 = and(_T_5, _T_9) @[SPadModule.scala 45:38]
    node _T_11 = add(padWriteIndexReg, UInt<1>("h1")) @[SPadModule.scala 48:79]
    node _T_12 = tail(_T_11, 1) @[SPadModule.scala 48:79]
    node writeWrapWire = _T_4 @[SPadModule.scala 13:43 SPadModule.scala 44:17]
    node _T_13 = mux(writeWrapWire, UInt<1>("h0"), _T_12) @[SPadModule.scala 48:41]
    node _T_14 = mux(writeFire, _T_13, padWriteIndexReg) @[SPadModule.scala 48:26]
    node _T_15 = mul(currentSlidingWire, UInt<1>("h1")) @[SPadModule.scala 62:37]
    node _T_16 = mul(_T_15, UInt<2>("h2")) @[SPadModule.scala 62:43]
    node _T_17 = mul(currentSlidingWire, UInt<1>("h1")) @[SPadModule.scala 63:37]
    node _T_18 = mul(_T_17, UInt<2>("h2")) @[SPadModule.scala 63:43]
    node _T_19 = mul(UInt<2>("h2"), UInt<3>("h4")) @[SPadModule.scala 63:57]
    node _T_20 = add(_T_18, _T_19) @[SPadModule.scala 63:50]
    node _T_21 = tail(_T_20, 1) @[SPadModule.scala 63:50]
    node _T_22 = sub(_T_21, UInt<1>("h1")) @[SPadModule.scala 63:63]
    node _T_23 = tail(_T_22, 1) @[SPadModule.scala 63:63]
    node _T_24 = add(currentSlidingWire, UInt<1>("h1")) @[SPadModule.scala 64:44]
    node _T_25 = tail(_T_24, 1) @[SPadModule.scala 64:44]
    node _T_26 = mul(_T_25, UInt<1>("h1")) @[SPadModule.scala 64:51]
    node _T_27 = mul(_T_26, UInt<2>("h2")) @[SPadModule.scala 64:57]
    node _T_28 = eq(futureLBStartReg, padReadIndexReg) @[SPadModule.scala 68:46]
    node _dataSPadReg_padReadIndexReg = _GEN_63 @[SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30]
    node _T_29 = bits(_dataSPadReg_padReadIndexReg, 3, 0) @[SPadModule.scala 116:30]
    node _T_30 = geq(_T_29, futureInActDataLB) @[SPadModule.scala 73:56]
    node _T_31 = sub(padReadIndexReg, UInt<1>("h1")) @[SPadModule.scala 74:55]
    node _T_32 = tail(_T_31, 1) @[SPadModule.scala 74:55]
    node _GEN_64 = validif(eq(UInt<1>("h0"), _T_32), dataSPadReg_0) @[SPadModule.scala 74:61]
    node _GEN_65 = mux(eq(UInt<1>("h1"), _T_32), dataSPadReg_1, _GEN_64) @[SPadModule.scala 74:61]
    node _GEN_66 = mux(eq(UInt<2>("h2"), _T_32), dataSPadReg_2, _GEN_65) @[SPadModule.scala 74:61]
    node _GEN_67 = mux(eq(UInt<2>("h3"), _T_32), dataSPadReg_3, _GEN_66) @[SPadModule.scala 74:61]
    node _GEN_68 = mux(eq(UInt<3>("h4"), _T_32), dataSPadReg_4, _GEN_67) @[SPadModule.scala 74:61]
    node _GEN_69 = mux(eq(UInt<3>("h5"), _T_32), dataSPadReg_5, _GEN_68) @[SPadModule.scala 74:61]
    node _GEN_70 = mux(eq(UInt<3>("h6"), _T_32), dataSPadReg_6, _GEN_69) @[SPadModule.scala 74:61]
    node _GEN_71 = mux(eq(UInt<3>("h7"), _T_32), dataSPadReg_7, _GEN_70) @[SPadModule.scala 74:61]
    node _GEN_72 = mux(eq(UInt<4>("h8"), _T_32), dataSPadReg_8, _GEN_71) @[SPadModule.scala 74:61]
    node _GEN_73 = mux(eq(UInt<4>("h9"), _T_32), dataSPadReg_9, _GEN_72) @[SPadModule.scala 74:61]
    node _GEN_74 = mux(eq(UInt<4>("ha"), _T_32), dataSPadReg_10, _GEN_73) @[SPadModule.scala 74:61]
    node _GEN_75 = mux(eq(UInt<4>("hb"), _T_32), dataSPadReg_11, _GEN_74) @[SPadModule.scala 74:61]
    node _GEN_76 = mux(eq(UInt<4>("hc"), _T_32), dataSPadReg_12, _GEN_75) @[SPadModule.scala 74:61]
    node _GEN_77 = mux(eq(UInt<4>("hd"), _T_32), dataSPadReg_13, _GEN_76) @[SPadModule.scala 74:61]
    node _GEN_78 = mux(eq(UInt<4>("he"), _T_32), dataSPadReg_14, _GEN_77) @[SPadModule.scala 74:61]
    node _GEN_79 = mux(eq(UInt<4>("hf"), _T_32), dataSPadReg_15, _GEN_78) @[SPadModule.scala 74:61]
    node _GEN_80 = mux(eq(UInt<5>("h10"), _T_32), dataSPadReg_16, _GEN_79) @[SPadModule.scala 74:61]
    node _GEN_81 = mux(eq(UInt<5>("h11"), _T_32), dataSPadReg_17, _GEN_80) @[SPadModule.scala 74:61]
    node _GEN_82 = mux(eq(UInt<5>("h12"), _T_32), dataSPadReg_18, _GEN_81) @[SPadModule.scala 74:61]
    node _GEN_83 = mux(eq(UInt<5>("h13"), _T_32), dataSPadReg_19, _GEN_82) @[SPadModule.scala 74:61]
    node _GEN_84 = mux(eq(UInt<5>("h14"), _T_32), dataSPadReg_20, _GEN_83) @[SPadModule.scala 74:61]
    node _GEN_85 = mux(eq(UInt<5>("h15"), _T_32), dataSPadReg_21, _GEN_84) @[SPadModule.scala 74:61]
    node _GEN_86 = mux(eq(UInt<5>("h16"), _T_32), dataSPadReg_22, _GEN_85) @[SPadModule.scala 74:61]
    node _GEN_87 = mux(eq(UInt<5>("h17"), _T_32), dataSPadReg_23, _GEN_86) @[SPadModule.scala 74:61]
    node _GEN_88 = mux(eq(UInt<5>("h18"), _T_32), dataSPadReg_24, _GEN_87) @[SPadModule.scala 74:61]
    node _GEN_89 = mux(eq(UInt<5>("h19"), _T_32), dataSPadReg_25, _GEN_88) @[SPadModule.scala 74:61]
    node _GEN_90 = mux(eq(UInt<5>("h1a"), _T_32), dataSPadReg_26, _GEN_89) @[SPadModule.scala 74:61]
    node _GEN_91 = mux(eq(UInt<5>("h1b"), _T_32), dataSPadReg_27, _GEN_90) @[SPadModule.scala 74:61]
    node _GEN_92 = mux(eq(UInt<5>("h1c"), _T_32), dataSPadReg_28, _GEN_91) @[SPadModule.scala 74:61]
    node _GEN_93 = mux(eq(UInt<5>("h1d"), _T_32), dataSPadReg_29, _GEN_92) @[SPadModule.scala 74:61]
    node _GEN_94 = mux(eq(UInt<5>("h1e"), _T_32), dataSPadReg_30, _GEN_93) @[SPadModule.scala 74:61]
    node _GEN_95 = mux(eq(UInt<5>("h1f"), _T_32), dataSPadReg_31, _GEN_94) @[SPadModule.scala 74:61]
    node _GEN_96 = mux(eq(UInt<6>("h20"), _T_32), dataSPadReg_32, _GEN_95) @[SPadModule.scala 74:61]
    node _GEN_97 = mux(eq(UInt<6>("h21"), _T_32), dataSPadReg_33, _GEN_96) @[SPadModule.scala 74:61]
    node _GEN_98 = mux(eq(UInt<6>("h22"), _T_32), dataSPadReg_34, _GEN_97) @[SPadModule.scala 74:61]
    node _GEN_99 = mux(eq(UInt<6>("h23"), _T_32), dataSPadReg_35, _GEN_98) @[SPadModule.scala 74:61]
    node _GEN_100 = mux(eq(UInt<6>("h24"), _T_32), dataSPadReg_36, _GEN_99) @[SPadModule.scala 74:61]
    node _GEN_101 = mux(eq(UInt<6>("h25"), _T_32), dataSPadReg_37, _GEN_100) @[SPadModule.scala 74:61]
    node _GEN_102 = mux(eq(UInt<6>("h26"), _T_32), dataSPadReg_38, _GEN_101) @[SPadModule.scala 74:61]
    node _GEN_103 = mux(eq(UInt<6>("h27"), _T_32), dataSPadReg_39, _GEN_102) @[SPadModule.scala 74:61]
    node _GEN_104 = mux(eq(UInt<6>("h28"), _T_32), dataSPadReg_40, _GEN_103) @[SPadModule.scala 74:61]
    node _GEN_105 = mux(eq(UInt<6>("h29"), _T_32), dataSPadReg_41, _GEN_104) @[SPadModule.scala 74:61]
    node _GEN_106 = mux(eq(UInt<6>("h2a"), _T_32), dataSPadReg_42, _GEN_105) @[SPadModule.scala 74:61]
    node _GEN_107 = mux(eq(UInt<6>("h2b"), _T_32), dataSPadReg_43, _GEN_106) @[SPadModule.scala 74:61]
    node _GEN_108 = mux(eq(UInt<6>("h2c"), _T_32), dataSPadReg_44, _GEN_107) @[SPadModule.scala 74:61]
    node _GEN_109 = mux(eq(UInt<6>("h2d"), _T_32), dataSPadReg_45, _GEN_108) @[SPadModule.scala 74:61]
    node _GEN_110 = mux(eq(UInt<6>("h2e"), _T_32), dataSPadReg_46, _GEN_109) @[SPadModule.scala 74:61]
    node _GEN_111 = mux(eq(UInt<6>("h2f"), _T_32), dataSPadReg_47, _GEN_110) @[SPadModule.scala 74:61]
    node _GEN_112 = mux(eq(UInt<6>("h30"), _T_32), dataSPadReg_48, _GEN_111) @[SPadModule.scala 74:61]
    node _GEN_113 = mux(eq(UInt<6>("h31"), _T_32), dataSPadReg_49, _GEN_112) @[SPadModule.scala 74:61]
    node _GEN_114 = mux(eq(UInt<6>("h32"), _T_32), dataSPadReg_50, _GEN_113) @[SPadModule.scala 74:61]
    node _GEN_115 = mux(eq(UInt<6>("h33"), _T_32), dataSPadReg_51, _GEN_114) @[SPadModule.scala 74:61]
    node _GEN_116 = mux(eq(UInt<6>("h34"), _T_32), dataSPadReg_52, _GEN_115) @[SPadModule.scala 74:61]
    node _GEN_117 = mux(eq(UInt<6>("h35"), _T_32), dataSPadReg_53, _GEN_116) @[SPadModule.scala 74:61]
    node _GEN_118 = mux(eq(UInt<6>("h36"), _T_32), dataSPadReg_54, _GEN_117) @[SPadModule.scala 74:61]
    node _GEN_119 = mux(eq(UInt<6>("h37"), _T_32), dataSPadReg_55, _GEN_118) @[SPadModule.scala 74:61]
    node _GEN_120 = mux(eq(UInt<6>("h38"), _T_32), dataSPadReg_56, _GEN_119) @[SPadModule.scala 74:61]
    node _GEN_121 = mux(eq(UInt<6>("h39"), _T_32), dataSPadReg_57, _GEN_120) @[SPadModule.scala 74:61]
    node _GEN_122 = mux(eq(UInt<6>("h3a"), _T_32), dataSPadReg_58, _GEN_121) @[SPadModule.scala 74:61]
    node _GEN_123 = mux(eq(UInt<6>("h3b"), _T_32), dataSPadReg_59, _GEN_122) @[SPadModule.scala 74:61]
    node _GEN_124 = mux(eq(UInt<6>("h3c"), _T_32), dataSPadReg_60, _GEN_123) @[SPadModule.scala 74:61]
    node _GEN_125 = mux(eq(UInt<6>("h3d"), _T_32), dataSPadReg_61, _GEN_124) @[SPadModule.scala 74:61]
    node _GEN_126 = mux(eq(UInt<6>("h3e"), _T_32), dataSPadReg_62, _GEN_125) @[SPadModule.scala 74:61]
    node _GEN_127 = mux(eq(UInt<6>("h3f"), _T_32), dataSPadReg_63, _GEN_126) @[SPadModule.scala 74:61]
    node _dataSPadReg_T_32 = _GEN_127 @[SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61 SPadModule.scala 74:61]
    node _T_33 = bits(_dataSPadReg_T_32, 3, 0) @[SPadModule.scala 74:61]
    node _T_34 = lt(_T_33, futureInActDataLB) @[SPadModule.scala 74:84]
    node _T_35 = eq(padReadIndexReg, UInt<1>("h0")) @[SPadModule.scala 74:125]
    node _T_36 = or(_T_34, _T_35) @[SPadModule.scala 74:105]
    node inActAdrLastReadInIdxWire = lastInActAdrDataReg @[SPadModule.scala 38:55 SPadModule.scala 89:29]
    node _T_37 = eq(padReadIndexReg, inActAdrLastReadInIdxWire) @[SPadModule.scala 75:22]
    node _T_38 = or(_T_36, _T_37) @[SPadModule.scala 74:134]
    node padEqMpy = io_ctrlPath_padEqMpyBool @[SPadModule.scala 30:37 SPadModule.scala 65:12]
    node weightIdxIncWire = io_ctrlPath_weightIdxInc @[SPadModule.scala 29:46 SPadModule.scala 59:20]
    node _T_39 = and(padEqMpy, weightIdxIncWire) @[SPadModule.scala 83:32]
    node inActColIncWire = io_ctrlPath_inActColInc @[SPadModule.scala 28:45 SPadModule.scala 58:19]
    node _T_40 = eq(inActColIncWire, UInt<1>("h0")) @[SPadModule.scala 83:56]
    node _T_41 = and(_T_39, _T_40) @[SPadModule.scala 83:52]
    node padEqWA = io_ctrlPath_padEqWABool @[SPadModule.scala 31:36 SPadModule.scala 66:11]
    node mightWeightZeroColumn = io_ctrlPath_mightWeightZeroColumn @[SPadModule.scala 35:50 SPadModule.scala 61:25]
    node _T_42 = and(padEqWA, mightWeightZeroColumn) @[SPadModule.scala 83:87]
    node _T_43 = or(_T_41, _T_42) @[SPadModule.scala 83:75]
    node _T_44 = add(padReadIndexReg, UInt<1>("h1")) @[SPadModule.scala 84:34]
    node _T_45 = tail(_T_44, 1) @[SPadModule.scala 84:34]
    node _GEN_128 = validif(eq(UInt<1>("h0"), _T_45), dataSPadReg_0) @[SPadModule.scala 84:40]
    node _GEN_129 = mux(eq(UInt<1>("h1"), _T_45), dataSPadReg_1, _GEN_128) @[SPadModule.scala 84:40]
    node _GEN_130 = mux(eq(UInt<2>("h2"), _T_45), dataSPadReg_2, _GEN_129) @[SPadModule.scala 84:40]
    node _GEN_131 = mux(eq(UInt<2>("h3"), _T_45), dataSPadReg_3, _GEN_130) @[SPadModule.scala 84:40]
    node _GEN_132 = mux(eq(UInt<3>("h4"), _T_45), dataSPadReg_4, _GEN_131) @[SPadModule.scala 84:40]
    node _GEN_133 = mux(eq(UInt<3>("h5"), _T_45), dataSPadReg_5, _GEN_132) @[SPadModule.scala 84:40]
    node _GEN_134 = mux(eq(UInt<3>("h6"), _T_45), dataSPadReg_6, _GEN_133) @[SPadModule.scala 84:40]
    node _GEN_135 = mux(eq(UInt<3>("h7"), _T_45), dataSPadReg_7, _GEN_134) @[SPadModule.scala 84:40]
    node _GEN_136 = mux(eq(UInt<4>("h8"), _T_45), dataSPadReg_8, _GEN_135) @[SPadModule.scala 84:40]
    node _GEN_137 = mux(eq(UInt<4>("h9"), _T_45), dataSPadReg_9, _GEN_136) @[SPadModule.scala 84:40]
    node _GEN_138 = mux(eq(UInt<4>("ha"), _T_45), dataSPadReg_10, _GEN_137) @[SPadModule.scala 84:40]
    node _GEN_139 = mux(eq(UInt<4>("hb"), _T_45), dataSPadReg_11, _GEN_138) @[SPadModule.scala 84:40]
    node _GEN_140 = mux(eq(UInt<4>("hc"), _T_45), dataSPadReg_12, _GEN_139) @[SPadModule.scala 84:40]
    node _GEN_141 = mux(eq(UInt<4>("hd"), _T_45), dataSPadReg_13, _GEN_140) @[SPadModule.scala 84:40]
    node _GEN_142 = mux(eq(UInt<4>("he"), _T_45), dataSPadReg_14, _GEN_141) @[SPadModule.scala 84:40]
    node _GEN_143 = mux(eq(UInt<4>("hf"), _T_45), dataSPadReg_15, _GEN_142) @[SPadModule.scala 84:40]
    node _GEN_144 = mux(eq(UInt<5>("h10"), _T_45), dataSPadReg_16, _GEN_143) @[SPadModule.scala 84:40]
    node _GEN_145 = mux(eq(UInt<5>("h11"), _T_45), dataSPadReg_17, _GEN_144) @[SPadModule.scala 84:40]
    node _GEN_146 = mux(eq(UInt<5>("h12"), _T_45), dataSPadReg_18, _GEN_145) @[SPadModule.scala 84:40]
    node _GEN_147 = mux(eq(UInt<5>("h13"), _T_45), dataSPadReg_19, _GEN_146) @[SPadModule.scala 84:40]
    node _GEN_148 = mux(eq(UInt<5>("h14"), _T_45), dataSPadReg_20, _GEN_147) @[SPadModule.scala 84:40]
    node _GEN_149 = mux(eq(UInt<5>("h15"), _T_45), dataSPadReg_21, _GEN_148) @[SPadModule.scala 84:40]
    node _GEN_150 = mux(eq(UInt<5>("h16"), _T_45), dataSPadReg_22, _GEN_149) @[SPadModule.scala 84:40]
    node _GEN_151 = mux(eq(UInt<5>("h17"), _T_45), dataSPadReg_23, _GEN_150) @[SPadModule.scala 84:40]
    node _GEN_152 = mux(eq(UInt<5>("h18"), _T_45), dataSPadReg_24, _GEN_151) @[SPadModule.scala 84:40]
    node _GEN_153 = mux(eq(UInt<5>("h19"), _T_45), dataSPadReg_25, _GEN_152) @[SPadModule.scala 84:40]
    node _GEN_154 = mux(eq(UInt<5>("h1a"), _T_45), dataSPadReg_26, _GEN_153) @[SPadModule.scala 84:40]
    node _GEN_155 = mux(eq(UInt<5>("h1b"), _T_45), dataSPadReg_27, _GEN_154) @[SPadModule.scala 84:40]
    node _GEN_156 = mux(eq(UInt<5>("h1c"), _T_45), dataSPadReg_28, _GEN_155) @[SPadModule.scala 84:40]
    node _GEN_157 = mux(eq(UInt<5>("h1d"), _T_45), dataSPadReg_29, _GEN_156) @[SPadModule.scala 84:40]
    node _GEN_158 = mux(eq(UInt<5>("h1e"), _T_45), dataSPadReg_30, _GEN_157) @[SPadModule.scala 84:40]
    node _GEN_159 = mux(eq(UInt<5>("h1f"), _T_45), dataSPadReg_31, _GEN_158) @[SPadModule.scala 84:40]
    node _GEN_160 = mux(eq(UInt<6>("h20"), _T_45), dataSPadReg_32, _GEN_159) @[SPadModule.scala 84:40]
    node _GEN_161 = mux(eq(UInt<6>("h21"), _T_45), dataSPadReg_33, _GEN_160) @[SPadModule.scala 84:40]
    node _GEN_162 = mux(eq(UInt<6>("h22"), _T_45), dataSPadReg_34, _GEN_161) @[SPadModule.scala 84:40]
    node _GEN_163 = mux(eq(UInt<6>("h23"), _T_45), dataSPadReg_35, _GEN_162) @[SPadModule.scala 84:40]
    node _GEN_164 = mux(eq(UInt<6>("h24"), _T_45), dataSPadReg_36, _GEN_163) @[SPadModule.scala 84:40]
    node _GEN_165 = mux(eq(UInt<6>("h25"), _T_45), dataSPadReg_37, _GEN_164) @[SPadModule.scala 84:40]
    node _GEN_166 = mux(eq(UInt<6>("h26"), _T_45), dataSPadReg_38, _GEN_165) @[SPadModule.scala 84:40]
    node _GEN_167 = mux(eq(UInt<6>("h27"), _T_45), dataSPadReg_39, _GEN_166) @[SPadModule.scala 84:40]
    node _GEN_168 = mux(eq(UInt<6>("h28"), _T_45), dataSPadReg_40, _GEN_167) @[SPadModule.scala 84:40]
    node _GEN_169 = mux(eq(UInt<6>("h29"), _T_45), dataSPadReg_41, _GEN_168) @[SPadModule.scala 84:40]
    node _GEN_170 = mux(eq(UInt<6>("h2a"), _T_45), dataSPadReg_42, _GEN_169) @[SPadModule.scala 84:40]
    node _GEN_171 = mux(eq(UInt<6>("h2b"), _T_45), dataSPadReg_43, _GEN_170) @[SPadModule.scala 84:40]
    node _GEN_172 = mux(eq(UInt<6>("h2c"), _T_45), dataSPadReg_44, _GEN_171) @[SPadModule.scala 84:40]
    node _GEN_173 = mux(eq(UInt<6>("h2d"), _T_45), dataSPadReg_45, _GEN_172) @[SPadModule.scala 84:40]
    node _GEN_174 = mux(eq(UInt<6>("h2e"), _T_45), dataSPadReg_46, _GEN_173) @[SPadModule.scala 84:40]
    node _GEN_175 = mux(eq(UInt<6>("h2f"), _T_45), dataSPadReg_47, _GEN_174) @[SPadModule.scala 84:40]
    node _GEN_176 = mux(eq(UInt<6>("h30"), _T_45), dataSPadReg_48, _GEN_175) @[SPadModule.scala 84:40]
    node _GEN_177 = mux(eq(UInt<6>("h31"), _T_45), dataSPadReg_49, _GEN_176) @[SPadModule.scala 84:40]
    node _GEN_178 = mux(eq(UInt<6>("h32"), _T_45), dataSPadReg_50, _GEN_177) @[SPadModule.scala 84:40]
    node _GEN_179 = mux(eq(UInt<6>("h33"), _T_45), dataSPadReg_51, _GEN_178) @[SPadModule.scala 84:40]
    node _GEN_180 = mux(eq(UInt<6>("h34"), _T_45), dataSPadReg_52, _GEN_179) @[SPadModule.scala 84:40]
    node _GEN_181 = mux(eq(UInt<6>("h35"), _T_45), dataSPadReg_53, _GEN_180) @[SPadModule.scala 84:40]
    node _GEN_182 = mux(eq(UInt<6>("h36"), _T_45), dataSPadReg_54, _GEN_181) @[SPadModule.scala 84:40]
    node _GEN_183 = mux(eq(UInt<6>("h37"), _T_45), dataSPadReg_55, _GEN_182) @[SPadModule.scala 84:40]
    node _GEN_184 = mux(eq(UInt<6>("h38"), _T_45), dataSPadReg_56, _GEN_183) @[SPadModule.scala 84:40]
    node _GEN_185 = mux(eq(UInt<6>("h39"), _T_45), dataSPadReg_57, _GEN_184) @[SPadModule.scala 84:40]
    node _GEN_186 = mux(eq(UInt<6>("h3a"), _T_45), dataSPadReg_58, _GEN_185) @[SPadModule.scala 84:40]
    node _GEN_187 = mux(eq(UInt<6>("h3b"), _T_45), dataSPadReg_59, _GEN_186) @[SPadModule.scala 84:40]
    node _GEN_188 = mux(eq(UInt<6>("h3c"), _T_45), dataSPadReg_60, _GEN_187) @[SPadModule.scala 84:40]
    node _GEN_189 = mux(eq(UInt<6>("h3d"), _T_45), dataSPadReg_61, _GEN_188) @[SPadModule.scala 84:40]
    node _GEN_190 = mux(eq(UInt<6>("h3e"), _T_45), dataSPadReg_62, _GEN_189) @[SPadModule.scala 84:40]
    node _GEN_191 = mux(eq(UInt<6>("h3f"), _T_45), dataSPadReg_63, _GEN_190) @[SPadModule.scala 84:40]
    node _dataSPadReg_T_45 = _GEN_191 @[SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40 SPadModule.scala 84:40]
    node _T_46 = bits(_dataSPadReg_T_45, 3, 0) @[SPadModule.scala 84:40]
    node _T_47 = gt(_T_46, inActDataUB) @[SPadModule.scala 84:63]
    node _T_48 = add(padReadIndexReg, UInt<1>("h1")) @[SPadModule.scala 84:98]
    node _T_49 = tail(_T_48, 1) @[SPadModule.scala 84:98]
    node inActAdrReadInIdxWire = pad(io_ctrlPath_readInIdx, 7) @[SPadModule.scala 36:51 SPadModule.scala 53:25]
    node _T_50 = lt(_T_49, inActAdrReadInIdxWire) @[SPadModule.scala 84:104]
    node _T_51 = and(_T_47, _T_50) @[SPadModule.scala 84:78]
    node _T_52 = add(padReadIndexReg, UInt<1>("h1")) @[SPadModule.scala 85:24]
    node _T_53 = tail(_T_52, 1) @[SPadModule.scala 85:24]
    node _T_54 = eq(_T_53, inActAdrReadInIdxWire) @[SPadModule.scala 85:30]
    node _T_55 = or(_T_51, _T_54) @[SPadModule.scala 84:130]
    node _T_56 = sub(UInt<2>("h2"), UInt<1>("h1")) @[SPadModule.scala 86:35]
    node _T_57 = tail(_T_56, 1) @[SPadModule.scala 86:35]
    node _T_58 = neq(currentSlidingWire, _T_57) @[SPadModule.scala 86:27]
    node _T_59 = and(_T_55, _T_58) @[SPadModule.scala 85:58]
    node _T_60 = and(_T_43, _T_59) @[SPadModule.scala 83:114]
    node _dataSPadReg_padReadIndexReg_0 = _GEN_63 @[SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30 SPadModule.scala 116:30]
    node _T_61 = bits(_dataSPadReg_padReadIndexReg_0, 3, 0) @[SPadModule.scala 116:30]
    node _T_62 = gt(_T_61, inActDataUB) @[SPadModule.scala 90:54]
    node _T_63 = eq(padReadIndexReg, UInt<4>("hc")) @[SPadModule.scala 91:42]
    node futureLBStartRegChange = _T_28 @[SPadModule.scala 39:52 SPadModule.scala 68:26]
    node _GEN_192 = mux(futureLBStartRegChange, inActAdrReadInIdxWire, lastInActAdrDataReg) @[SPadModule.scala 93:31]
    node _dataSPadReg_padWriteIndexReg = io_dataPath_writeInData_data_bits @[SPadModule.scala 98:32 SPadModule.scala 98:32]
    node _GEN_193 = mux(eq(UInt<1>("h0"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_0) @[SPadModule.scala 98:32]
    node _GEN_194 = mux(eq(UInt<1>("h1"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_1) @[SPadModule.scala 98:32]
    node _GEN_195 = mux(eq(UInt<2>("h2"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_2) @[SPadModule.scala 98:32]
    node _GEN_196 = mux(eq(UInt<2>("h3"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_3) @[SPadModule.scala 98:32]
    node _GEN_197 = mux(eq(UInt<3>("h4"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_4) @[SPadModule.scala 98:32]
    node _GEN_198 = mux(eq(UInt<3>("h5"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_5) @[SPadModule.scala 98:32]
    node _GEN_199 = mux(eq(UInt<3>("h6"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_6) @[SPadModule.scala 98:32]
    node _GEN_200 = mux(eq(UInt<3>("h7"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_7) @[SPadModule.scala 98:32]
    node _GEN_201 = mux(eq(UInt<4>("h8"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_8) @[SPadModule.scala 98:32]
    node _GEN_202 = mux(eq(UInt<4>("h9"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_9) @[SPadModule.scala 98:32]
    node _GEN_203 = mux(eq(UInt<4>("ha"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_10) @[SPadModule.scala 98:32]
    node _GEN_204 = mux(eq(UInt<4>("hb"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_11) @[SPadModule.scala 98:32]
    node _GEN_205 = mux(eq(UInt<4>("hc"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_12) @[SPadModule.scala 98:32]
    node _GEN_206 = mux(eq(UInt<4>("hd"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_13) @[SPadModule.scala 98:32]
    node _GEN_207 = mux(eq(UInt<4>("he"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_14) @[SPadModule.scala 98:32]
    node _GEN_208 = mux(eq(UInt<4>("hf"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_15) @[SPadModule.scala 98:32]
    node _GEN_209 = mux(eq(UInt<5>("h10"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_16) @[SPadModule.scala 98:32]
    node _GEN_210 = mux(eq(UInt<5>("h11"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_17) @[SPadModule.scala 98:32]
    node _GEN_211 = mux(eq(UInt<5>("h12"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_18) @[SPadModule.scala 98:32]
    node _GEN_212 = mux(eq(UInt<5>("h13"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_19) @[SPadModule.scala 98:32]
    node _GEN_213 = mux(eq(UInt<5>("h14"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_20) @[SPadModule.scala 98:32]
    node _GEN_214 = mux(eq(UInt<5>("h15"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_21) @[SPadModule.scala 98:32]
    node _GEN_215 = mux(eq(UInt<5>("h16"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_22) @[SPadModule.scala 98:32]
    node _GEN_216 = mux(eq(UInt<5>("h17"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_23) @[SPadModule.scala 98:32]
    node _GEN_217 = mux(eq(UInt<5>("h18"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_24) @[SPadModule.scala 98:32]
    node _GEN_218 = mux(eq(UInt<5>("h19"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_25) @[SPadModule.scala 98:32]
    node _GEN_219 = mux(eq(UInt<5>("h1a"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_26) @[SPadModule.scala 98:32]
    node _GEN_220 = mux(eq(UInt<5>("h1b"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_27) @[SPadModule.scala 98:32]
    node _GEN_221 = mux(eq(UInt<5>("h1c"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_28) @[SPadModule.scala 98:32]
    node _GEN_222 = mux(eq(UInt<5>("h1d"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_29) @[SPadModule.scala 98:32]
    node _GEN_223 = mux(eq(UInt<5>("h1e"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_30) @[SPadModule.scala 98:32]
    node _GEN_224 = mux(eq(UInt<5>("h1f"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_31) @[SPadModule.scala 98:32]
    node _GEN_225 = mux(eq(UInt<6>("h20"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_32) @[SPadModule.scala 98:32]
    node _GEN_226 = mux(eq(UInt<6>("h21"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_33) @[SPadModule.scala 98:32]
    node _GEN_227 = mux(eq(UInt<6>("h22"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_34) @[SPadModule.scala 98:32]
    node _GEN_228 = mux(eq(UInt<6>("h23"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_35) @[SPadModule.scala 98:32]
    node _GEN_229 = mux(eq(UInt<6>("h24"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_36) @[SPadModule.scala 98:32]
    node _GEN_230 = mux(eq(UInt<6>("h25"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_37) @[SPadModule.scala 98:32]
    node _GEN_231 = mux(eq(UInt<6>("h26"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_38) @[SPadModule.scala 98:32]
    node _GEN_232 = mux(eq(UInt<6>("h27"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_39) @[SPadModule.scala 98:32]
    node _GEN_233 = mux(eq(UInt<6>("h28"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_40) @[SPadModule.scala 98:32]
    node _GEN_234 = mux(eq(UInt<6>("h29"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_41) @[SPadModule.scala 98:32]
    node _GEN_235 = mux(eq(UInt<6>("h2a"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_42) @[SPadModule.scala 98:32]
    node _GEN_236 = mux(eq(UInt<6>("h2b"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_43) @[SPadModule.scala 98:32]
    node _GEN_237 = mux(eq(UInt<6>("h2c"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_44) @[SPadModule.scala 98:32]
    node _GEN_238 = mux(eq(UInt<6>("h2d"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_45) @[SPadModule.scala 98:32]
    node _GEN_239 = mux(eq(UInt<6>("h2e"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_46) @[SPadModule.scala 98:32]
    node _GEN_240 = mux(eq(UInt<6>("h2f"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_47) @[SPadModule.scala 98:32]
    node _GEN_241 = mux(eq(UInt<6>("h30"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_48) @[SPadModule.scala 98:32]
    node _GEN_242 = mux(eq(UInt<6>("h31"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_49) @[SPadModule.scala 98:32]
    node _GEN_243 = mux(eq(UInt<6>("h32"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_50) @[SPadModule.scala 98:32]
    node _GEN_244 = mux(eq(UInt<6>("h33"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_51) @[SPadModule.scala 98:32]
    node _GEN_245 = mux(eq(UInt<6>("h34"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_52) @[SPadModule.scala 98:32]
    node _GEN_246 = mux(eq(UInt<6>("h35"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_53) @[SPadModule.scala 98:32]
    node _GEN_247 = mux(eq(UInt<6>("h36"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_54) @[SPadModule.scala 98:32]
    node _GEN_248 = mux(eq(UInt<6>("h37"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_55) @[SPadModule.scala 98:32]
    node _GEN_249 = mux(eq(UInt<6>("h38"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_56) @[SPadModule.scala 98:32]
    node _GEN_250 = mux(eq(UInt<6>("h39"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_57) @[SPadModule.scala 98:32]
    node _GEN_251 = mux(eq(UInt<6>("h3a"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_58) @[SPadModule.scala 98:32]
    node _GEN_252 = mux(eq(UInt<6>("h3b"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_59) @[SPadModule.scala 98:32]
    node _GEN_253 = mux(eq(UInt<6>("h3c"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_60) @[SPadModule.scala 98:32]
    node _GEN_254 = mux(eq(UInt<6>("h3d"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_61) @[SPadModule.scala 98:32]
    node _GEN_255 = mux(eq(UInt<6>("h3e"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_62) @[SPadModule.scala 98:32]
    node _GEN_256 = mux(eq(UInt<6>("h3f"), padWriteIndexReg), _dataSPadReg_padWriteIndexReg, dataSPadReg_63) @[SPadModule.scala 98:32]
    node _GEN_257 = mux(writeFire, _GEN_193, dataSPadReg_0) @[SPadModule.scala 97:19]
    node _GEN_258 = mux(writeFire, _GEN_194, dataSPadReg_1) @[SPadModule.scala 97:19]
    node _GEN_259 = mux(writeFire, _GEN_195, dataSPadReg_2) @[SPadModule.scala 97:19]
    node _GEN_260 = mux(writeFire, _GEN_196, dataSPadReg_3) @[SPadModule.scala 97:19]
    node _GEN_261 = mux(writeFire, _GEN_197, dataSPadReg_4) @[SPadModule.scala 97:19]
    node _GEN_262 = mux(writeFire, _GEN_198, dataSPadReg_5) @[SPadModule.scala 97:19]
    node _GEN_263 = mux(writeFire, _GEN_199, dataSPadReg_6) @[SPadModule.scala 97:19]
    node _GEN_264 = mux(writeFire, _GEN_200, dataSPadReg_7) @[SPadModule.scala 97:19]
    node _GEN_265 = mux(writeFire, _GEN_201, dataSPadReg_8) @[SPadModule.scala 97:19]
    node _GEN_266 = mux(writeFire, _GEN_202, dataSPadReg_9) @[SPadModule.scala 97:19]
    node _GEN_267 = mux(writeFire, _GEN_203, dataSPadReg_10) @[SPadModule.scala 97:19]
    node _GEN_268 = mux(writeFire, _GEN_204, dataSPadReg_11) @[SPadModule.scala 97:19]
    node _GEN_269 = mux(writeFire, _GEN_205, dataSPadReg_12) @[SPadModule.scala 97:19]
    node _GEN_270 = mux(writeFire, _GEN_206, dataSPadReg_13) @[SPadModule.scala 97:19]
    node _GEN_271 = mux(writeFire, _GEN_207, dataSPadReg_14) @[SPadModule.scala 97:19]
    node _GEN_272 = mux(writeFire, _GEN_208, dataSPadReg_15) @[SPadModule.scala 97:19]
    node _GEN_273 = mux(writeFire, _GEN_209, dataSPadReg_16) @[SPadModule.scala 97:19]
    node _GEN_274 = mux(writeFire, _GEN_210, dataSPadReg_17) @[SPadModule.scala 97:19]
    node _GEN_275 = mux(writeFire, _GEN_211, dataSPadReg_18) @[SPadModule.scala 97:19]
    node _GEN_276 = mux(writeFire, _GEN_212, dataSPadReg_19) @[SPadModule.scala 97:19]
    node _GEN_277 = mux(writeFire, _GEN_213, dataSPadReg_20) @[SPadModule.scala 97:19]
    node _GEN_278 = mux(writeFire, _GEN_214, dataSPadReg_21) @[SPadModule.scala 97:19]
    node _GEN_279 = mux(writeFire, _GEN_215, dataSPadReg_22) @[SPadModule.scala 97:19]
    node _GEN_280 = mux(writeFire, _GEN_216, dataSPadReg_23) @[SPadModule.scala 97:19]
    node _GEN_281 = mux(writeFire, _GEN_217, dataSPadReg_24) @[SPadModule.scala 97:19]
    node _GEN_282 = mux(writeFire, _GEN_218, dataSPadReg_25) @[SPadModule.scala 97:19]
    node _GEN_283 = mux(writeFire, _GEN_219, dataSPadReg_26) @[SPadModule.scala 97:19]
    node _GEN_284 = mux(writeFire, _GEN_220, dataSPadReg_27) @[SPadModule.scala 97:19]
    node _GEN_285 = mux(writeFire, _GEN_221, dataSPadReg_28) @[SPadModule.scala 97:19]
    node _GEN_286 = mux(writeFire, _GEN_222, dataSPadReg_29) @[SPadModule.scala 97:19]
    node _GEN_287 = mux(writeFire, _GEN_223, dataSPadReg_30) @[SPadModule.scala 97:19]
    node _GEN_288 = mux(writeFire, _GEN_224, dataSPadReg_31) @[SPadModule.scala 97:19]
    node _GEN_289 = mux(writeFire, _GEN_225, dataSPadReg_32) @[SPadModule.scala 97:19]
    node _GEN_290 = mux(writeFire, _GEN_226, dataSPadReg_33) @[SPadModule.scala 97:19]
    node _GEN_291 = mux(writeFire, _GEN_227, dataSPadReg_34) @[SPadModule.scala 97:19]
    node _GEN_292 = mux(writeFire, _GEN_228, dataSPadReg_35) @[SPadModule.scala 97:19]
    node _GEN_293 = mux(writeFire, _GEN_229, dataSPadReg_36) @[SPadModule.scala 97:19]
    node _GEN_294 = mux(writeFire, _GEN_230, dataSPadReg_37) @[SPadModule.scala 97:19]
    node _GEN_295 = mux(writeFire, _GEN_231, dataSPadReg_38) @[SPadModule.scala 97:19]
    node _GEN_296 = mux(writeFire, _GEN_232, dataSPadReg_39) @[SPadModule.scala 97:19]
    node _GEN_297 = mux(writeFire, _GEN_233, dataSPadReg_40) @[SPadModule.scala 97:19]
    node _GEN_298 = mux(writeFire, _GEN_234, dataSPadReg_41) @[SPadModule.scala 97:19]
    node _GEN_299 = mux(writeFire, _GEN_235, dataSPadReg_42) @[SPadModule.scala 97:19]
    node _GEN_300 = mux(writeFire, _GEN_236, dataSPadReg_43) @[SPadModule.scala 97:19]
    node _GEN_301 = mux(writeFire, _GEN_237, dataSPadReg_44) @[SPadModule.scala 97:19]
    node _GEN_302 = mux(writeFire, _GEN_238, dataSPadReg_45) @[SPadModule.scala 97:19]
    node _GEN_303 = mux(writeFire, _GEN_239, dataSPadReg_46) @[SPadModule.scala 97:19]
    node _GEN_304 = mux(writeFire, _GEN_240, dataSPadReg_47) @[SPadModule.scala 97:19]
    node _GEN_305 = mux(writeFire, _GEN_241, dataSPadReg_48) @[SPadModule.scala 97:19]
    node _GEN_306 = mux(writeFire, _GEN_242, dataSPadReg_49) @[SPadModule.scala 97:19]
    node _GEN_307 = mux(writeFire, _GEN_243, dataSPadReg_50) @[SPadModule.scala 97:19]
    node _GEN_308 = mux(writeFire, _GEN_244, dataSPadReg_51) @[SPadModule.scala 97:19]
    node _GEN_309 = mux(writeFire, _GEN_245, dataSPadReg_52) @[SPadModule.scala 97:19]
    node _GEN_310 = mux(writeFire, _GEN_246, dataSPadReg_53) @[SPadModule.scala 97:19]
    node _GEN_311 = mux(writeFire, _GEN_247, dataSPadReg_54) @[SPadModule.scala 97:19]
    node _GEN_312 = mux(writeFire, _GEN_248, dataSPadReg_55) @[SPadModule.scala 97:19]
    node _GEN_313 = mux(writeFire, _GEN_249, dataSPadReg_56) @[SPadModule.scala 97:19]
    node _GEN_314 = mux(writeFire, _GEN_250, dataSPadReg_57) @[SPadModule.scala 97:19]
    node _GEN_315 = mux(writeFire, _GEN_251, dataSPadReg_58) @[SPadModule.scala 97:19]
    node _GEN_316 = mux(writeFire, _GEN_252, dataSPadReg_59) @[SPadModule.scala 97:19]
    node _GEN_317 = mux(writeFire, _GEN_253, dataSPadReg_60) @[SPadModule.scala 97:19]
    node _GEN_318 = mux(writeFire, _GEN_254, dataSPadReg_61) @[SPadModule.scala 97:19]
    node _GEN_319 = mux(writeFire, _GEN_255, dataSPadReg_62) @[SPadModule.scala 97:19]
    node _GEN_320 = mux(writeFire, _GEN_256, dataSPadReg_63) @[SPadModule.scala 97:19]
    node _T_64 = and(currentCountAtFutureLB, lastCountNotAtFutureLB) @[SPadModule.scala 105:36]
    node _GEN_321 = mux(_T_64, padReadIndexReg, futureLBStartReg) @[SPadModule.scala 105:63]
    node readWrapWire = _T_10 @[SPadModule.scala 14:42 SPadModule.scala 45:16]
    node _GEN_322 = mux(readWrapWire, UInt<1>("h0"), padReadIndexReg) @[SPadModule.scala 108:27]
    node _T_65 = add(padReadIndexReg, UInt<1>("h1")) @[SPadModule.scala 111:43]
    node _T_66 = tail(_T_65, 1) @[SPadModule.scala 111:43]
    node _GEN_323 = mux(readSlidingInc, futureLBStartReg, _T_66) @[SPadModule.scala 102:27]
    node _GEN_324 = mux(readSlidingInc, futureLBStartReg, _GEN_321) @[SPadModule.scala 102:27]
    node readIndexInc = io_ctrlPath_indexInc @[SPadModule.scala 15:42 SPadModule.scala 119:16]
    node _GEN_325 = mux(readIndexInc, _GEN_323, padReadIndexReg) @[SPadModule.scala 101:23]
    node _GEN_326 = mux(readIndexInc, _GEN_324, futureLBStartReg) @[SPadModule.scala 101:23]
    node _T__0 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__1 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__2 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__3 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__4 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__5 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__6 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__7 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__8 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__9 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__10 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__11 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__12 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__13 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__14 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__15 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__16 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__17 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__18 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__19 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__20 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__21 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__22 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__23 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__24 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__25 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__26 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__27 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__28 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__29 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__30 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__31 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__32 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__33 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__34 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__35 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__36 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__37 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__38 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__39 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__40 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__41 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__42 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__43 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__44 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__45 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__46 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__47 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__48 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__49 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__50 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__51 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__52 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__53 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__54 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__55 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__56 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__57 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__58 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__59 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__60 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__61 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__62 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node _T__63 = UInt<12>("h0") @[SPadModule.scala 23:54 SPadModule.scala 23:54]
    node padEqID = io_ctrlPath_padEqIDBool @[SPadModule.scala 32:36 SPadModule.scala 67:11]
    node inActReadIdxBeyondUB = _T_62 @[SPadModule.scala 40:50 SPadModule.scala 90:24]
    node inActLastNonZeroEle = _T_63 @[SPadModule.scala 41:48 SPadModule.scala 91:23]
    io_dataPath_columnNum <= padReadIndexReg @[SPadModule.scala 54:25]
    io_dataPath_readOutData <= dataWire @[SPadModule.scala 120:27]
    io_dataPath_writeInData_data_ready <= UInt<1>("h1") @[SPadModule.scala 47:25]
    io_dataPath_slidingBoxUB <= inActDataUB @[SPadModule.scala 56:28]
    io_dataPath_slidingBoxLB <= inActDataLB @[SPadModule.scala 55:28]
    io_dataPath_futureLBStart <= futureLBStartReg @[SPadModule.scala 57:29]
    io_ctrlPath_writeIdx <= padWriteIndexReg @[SPadModule.scala 51:24]
    io_ctrlPath_writeFin <= writeWrapWire @[SPadModule.scala 52:24]
    io_ctrlPath_slidingInc <= readSlidingInc @[SPadModule.scala 88:26]
    io_ctrlPath_inActReadIdxBeyondUB <= inActReadIdxBeyondUB @[SPadModule.scala 69:36]
    io_ctrlPath_inActLastNonZeroEle <= inActLastNonZeroEle @[SPadModule.scala 70:35]
    padWriteIndexReg <= mux(reset, UInt<6>("h0"), _T_14) @[SPadModule.scala 48:20]
    padReadIndexReg <= mux(reset, UInt<6>("h0"), _GEN_325) @[SPadModule.scala 103:24 SPadModule.scala 109:26 SPadModule.scala 111:24]
    readSlidingInc <= mux(reset, UInt<1>("h0"), _T_60) @[SPadModule.scala 83:18]
    inActDataLB <= mux(reset, UInt<4>("h0"), _T_16) @[SPadModule.scala 62:15]
    inActDataUB <= mux(reset, UInt<4>("h0"), bits(_T_23, 3, 0)) @[SPadModule.scala 63:15]
    futureInActDataLB <= mux(reset, UInt<4>("h0"), _T_27) @[SPadModule.scala 64:21]
    dataSPadReg_0 <= mux(reset, _T__0, _GEN_257) @[SPadModule.scala 98:32]
    dataSPadReg_1 <= mux(reset, _T__1, _GEN_258) @[SPadModule.scala 98:32]
    dataSPadReg_2 <= mux(reset, _T__2, _GEN_259) @[SPadModule.scala 98:32]
    dataSPadReg_3 <= mux(reset, _T__3, _GEN_260) @[SPadModule.scala 98:32]
    dataSPadReg_4 <= mux(reset, _T__4, _GEN_261) @[SPadModule.scala 98:32]
    dataSPadReg_5 <= mux(reset, _T__5, _GEN_262) @[SPadModule.scala 98:32]
    dataSPadReg_6 <= mux(reset, _T__6, _GEN_263) @[SPadModule.scala 98:32]
    dataSPadReg_7 <= mux(reset, _T__7, _GEN_264) @[SPadModule.scala 98:32]
    dataSPadReg_8 <= mux(reset, _T__8, _GEN_265) @[SPadModule.scala 98:32]
    dataSPadReg_9 <= mux(reset, _T__9, _GEN_266) @[SPadModule.scala 98:32]
    dataSPadReg_10 <= mux(reset, _T__10, _GEN_267) @[SPadModule.scala 98:32]
    dataSPadReg_11 <= mux(reset, _T__11, _GEN_268) @[SPadModule.scala 98:32]
    dataSPadReg_12 <= mux(reset, _T__12, _GEN_269) @[SPadModule.scala 98:32]
    dataSPadReg_13 <= mux(reset, _T__13, _GEN_270) @[SPadModule.scala 98:32]
    dataSPadReg_14 <= mux(reset, _T__14, _GEN_271) @[SPadModule.scala 98:32]
    dataSPadReg_15 <= mux(reset, _T__15, _GEN_272) @[SPadModule.scala 98:32]
    dataSPadReg_16 <= mux(reset, _T__16, _GEN_273) @[SPadModule.scala 98:32]
    dataSPadReg_17 <= mux(reset, _T__17, _GEN_274) @[SPadModule.scala 98:32]
    dataSPadReg_18 <= mux(reset, _T__18, _GEN_275) @[SPadModule.scala 98:32]
    dataSPadReg_19 <= mux(reset, _T__19, _GEN_276) @[SPadModule.scala 98:32]
    dataSPadReg_20 <= mux(reset, _T__20, _GEN_277) @[SPadModule.scala 98:32]
    dataSPadReg_21 <= mux(reset, _T__21, _GEN_278) @[SPadModule.scala 98:32]
    dataSPadReg_22 <= mux(reset, _T__22, _GEN_279) @[SPadModule.scala 98:32]
    dataSPadReg_23 <= mux(reset, _T__23, _GEN_280) @[SPadModule.scala 98:32]
    dataSPadReg_24 <= mux(reset, _T__24, _GEN_281) @[SPadModule.scala 98:32]
    dataSPadReg_25 <= mux(reset, _T__25, _GEN_282) @[SPadModule.scala 98:32]
    dataSPadReg_26 <= mux(reset, _T__26, _GEN_283) @[SPadModule.scala 98:32]
    dataSPadReg_27 <= mux(reset, _T__27, _GEN_284) @[SPadModule.scala 98:32]
    dataSPadReg_28 <= mux(reset, _T__28, _GEN_285) @[SPadModule.scala 98:32]
    dataSPadReg_29 <= mux(reset, _T__29, _GEN_286) @[SPadModule.scala 98:32]
    dataSPadReg_30 <= mux(reset, _T__30, _GEN_287) @[SPadModule.scala 98:32]
    dataSPadReg_31 <= mux(reset, _T__31, _GEN_288) @[SPadModule.scala 98:32]
    dataSPadReg_32 <= mux(reset, _T__32, _GEN_289) @[SPadModule.scala 98:32]
    dataSPadReg_33 <= mux(reset, _T__33, _GEN_290) @[SPadModule.scala 98:32]
    dataSPadReg_34 <= mux(reset, _T__34, _GEN_291) @[SPadModule.scala 98:32]
    dataSPadReg_35 <= mux(reset, _T__35, _GEN_292) @[SPadModule.scala 98:32]
    dataSPadReg_36 <= mux(reset, _T__36, _GEN_293) @[SPadModule.scala 98:32]
    dataSPadReg_37 <= mux(reset, _T__37, _GEN_294) @[SPadModule.scala 98:32]
    dataSPadReg_38 <= mux(reset, _T__38, _GEN_295) @[SPadModule.scala 98:32]
    dataSPadReg_39 <= mux(reset, _T__39, _GEN_296) @[SPadModule.scala 98:32]
    dataSPadReg_40 <= mux(reset, _T__40, _GEN_297) @[SPadModule.scala 98:32]
    dataSPadReg_41 <= mux(reset, _T__41, _GEN_298) @[SPadModule.scala 98:32]
    dataSPadReg_42 <= mux(reset, _T__42, _GEN_299) @[SPadModule.scala 98:32]
    dataSPadReg_43 <= mux(reset, _T__43, _GEN_300) @[SPadModule.scala 98:32]
    dataSPadReg_44 <= mux(reset, _T__44, _GEN_301) @[SPadModule.scala 98:32]
    dataSPadReg_45 <= mux(reset, _T__45, _GEN_302) @[SPadModule.scala 98:32]
    dataSPadReg_46 <= mux(reset, _T__46, _GEN_303) @[SPadModule.scala 98:32]
    dataSPadReg_47 <= mux(reset, _T__47, _GEN_304) @[SPadModule.scala 98:32]
    dataSPadReg_48 <= mux(reset, _T__48, _GEN_305) @[SPadModule.scala 98:32]
    dataSPadReg_49 <= mux(reset, _T__49, _GEN_306) @[SPadModule.scala 98:32]
    dataSPadReg_50 <= mux(reset, _T__50, _GEN_307) @[SPadModule.scala 98:32]
    dataSPadReg_51 <= mux(reset, _T__51, _GEN_308) @[SPadModule.scala 98:32]
    dataSPadReg_52 <= mux(reset, _T__52, _GEN_309) @[SPadModule.scala 98:32]
    dataSPadReg_53 <= mux(reset, _T__53, _GEN_310) @[SPadModule.scala 98:32]
    dataSPadReg_54 <= mux(reset, _T__54, _GEN_311) @[SPadModule.scala 98:32]
    dataSPadReg_55 <= mux(reset, _T__55, _GEN_312) @[SPadModule.scala 98:32]
    dataSPadReg_56 <= mux(reset, _T__56, _GEN_313) @[SPadModule.scala 98:32]
    dataSPadReg_57 <= mux(reset, _T__57, _GEN_314) @[SPadModule.scala 98:32]
    dataSPadReg_58 <= mux(reset, _T__58, _GEN_315) @[SPadModule.scala 98:32]
    dataSPadReg_59 <= mux(reset, _T__59, _GEN_316) @[SPadModule.scala 98:32]
    dataSPadReg_60 <= mux(reset, _T__60, _GEN_317) @[SPadModule.scala 98:32]
    dataSPadReg_61 <= mux(reset, _T__61, _GEN_318) @[SPadModule.scala 98:32]
    dataSPadReg_62 <= mux(reset, _T__62, _GEN_319) @[SPadModule.scala 98:32]
    dataSPadReg_63 <= mux(reset, _T__63, _GEN_320) @[SPadModule.scala 98:32]
    currentColTopIdxReg <= mux(reset, UInt<4>("h0"), currentColTopIdxReg)
    futureLBStartReg <= mux(reset, UInt<4>("h0"), bits(_GEN_326, 3, 0)) @[SPadModule.scala 106:27]
    currentCountAtFutureLB <= mux(reset, UInt<1>("h0"), _T_30) @[SPadModule.scala 73:26]
    lastCountNotAtFutureLB <= mux(reset, UInt<1>("h0"), _T_38) @[SPadModule.scala 74:26]
    lastInActAdrDataReg <= mux(reset, UInt<7>("h0"), _GEN_192) @[SPadModule.scala 94:25]

  module WeightSPadAdrModule :
    input clock : Clock
    input reset : UInt<1>
    output io_dataPath_columnNum : UInt<4>
    output io_dataPath_readOutData : UInt<7>
    output io_dataPath_writeInData_data_ready : UInt<1>
    input io_dataPath_writeInData_data_valid : UInt<1>
    input io_dataPath_writeInData_data_bits : UInt<7>
    input io_ctrlPath_writeEn : UInt<1>
    output io_ctrlPath_writeIdx : UInt<4>
    output io_ctrlPath_writeFin : UInt<1>
    input io_ctrlPath_readEn : UInt<1>
    input io_ctrlPath_readInIdx : UInt<4>
    input io_ctrlPath_indexInc : UInt<1>
    input io_ctrlPath_readInIdxEn : UInt<1>
  
    reg padWriteIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padWriteIndexReg) @[SPadModule.scala 183:49]
    reg padReadIndexReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), padReadIndexReg) @[SPadModule.scala 184:48]
    node _T = and(io_dataPath_writeInData_data_ready, io_dataPath_writeInData_data_valid) @[Decoupled.scala 40:37]
    node _T_1 = and(_T, io_ctrlPath_writeEn) @[SPadModule.scala 189:39]
    node _T_2 = eq(io_dataPath_writeInData_data_bits, UInt<1>("h0")) @[SPadModule.scala 190:41]
    node writeFire = _T_1 @[SPadModule.scala 188:39 SPadModule.scala 189:13]
    node _T_3 = and(_T_2, writeFire) @[SPadModule.scala 190:49]
    reg addressSPad_15 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addressSPad_15) @[SPadModule.scala 133:45]
    reg addressSPad_14 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addressSPad_14) @[SPadModule.scala 133:45]
    reg addressSPad_13 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addressSPad_13) @[SPadModule.scala 133:45]
    reg addressSPad_12 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addressSPad_12) @[SPadModule.scala 133:45]
    reg addressSPad_11 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addressSPad_11) @[SPadModule.scala 133:45]
    reg addressSPad_10 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addressSPad_10) @[SPadModule.scala 133:45]
    reg addressSPad_9 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addressSPad_9) @[SPadModule.scala 133:45]
    reg addressSPad_8 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addressSPad_8) @[SPadModule.scala 133:45]
    reg addressSPad_7 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addressSPad_7) @[SPadModule.scala 133:45]
    reg addressSPad_6 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addressSPad_6) @[SPadModule.scala 133:45]
    reg addressSPad_5 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addressSPad_5) @[SPadModule.scala 133:45]
    reg addressSPad_4 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addressSPad_4) @[SPadModule.scala 133:45]
    reg addressSPad_3 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addressSPad_3) @[SPadModule.scala 133:45]
    reg addressSPad_2 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addressSPad_2) @[SPadModule.scala 133:45]
    reg addressSPad_1 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addressSPad_1) @[SPadModule.scala 133:45]
    reg addressSPad_0 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), addressSPad_0) @[SPadModule.scala 133:45]
    node _GEN_34 = validif(eq(UInt<1>("h0"), padReadIndexReg), addressSPad_0) @[SPadModule.scala 142:12]
    node _GEN_35 = mux(eq(UInt<1>("h1"), padReadIndexReg), addressSPad_1, _GEN_34) @[SPadModule.scala 142:12]
    node _GEN_36 = mux(eq(UInt<2>("h2"), padReadIndexReg), addressSPad_2, _GEN_35) @[SPadModule.scala 142:12]
    node _GEN_37 = mux(eq(UInt<2>("h3"), padReadIndexReg), addressSPad_3, _GEN_36) @[SPadModule.scala 142:12]
    node _GEN_38 = mux(eq(UInt<3>("h4"), padReadIndexReg), addressSPad_4, _GEN_37) @[SPadModule.scala 142:12]
    node _GEN_39 = mux(eq(UInt<3>("h5"), padReadIndexReg), addressSPad_5, _GEN_38) @[SPadModule.scala 142:12]
    node _GEN_40 = mux(eq(UInt<3>("h6"), padReadIndexReg), addressSPad_6, _GEN_39) @[SPadModule.scala 142:12]
    node _GEN_41 = mux(eq(UInt<3>("h7"), padReadIndexReg), addressSPad_7, _GEN_40) @[SPadModule.scala 142:12]
    node _GEN_42 = mux(eq(UInt<4>("h8"), padReadIndexReg), addressSPad_8, _GEN_41) @[SPadModule.scala 142:12]
    node _GEN_43 = mux(eq(UInt<4>("h9"), padReadIndexReg), addressSPad_9, _GEN_42) @[SPadModule.scala 142:12]
    node _GEN_44 = mux(eq(UInt<4>("ha"), padReadIndexReg), addressSPad_10, _GEN_43) @[SPadModule.scala 142:12]
    node _GEN_45 = mux(eq(UInt<4>("hb"), padReadIndexReg), addressSPad_11, _GEN_44) @[SPadModule.scala 142:12]
    node _GEN_46 = mux(eq(UInt<4>("hc"), padReadIndexReg), addressSPad_12, _GEN_45) @[SPadModule.scala 142:12]
    node _GEN_47 = mux(eq(UInt<4>("hd"), padReadIndexReg), addressSPad_13, _GEN_46) @[SPadModule.scala 142:12]
    node _GEN_48 = mux(eq(UInt<4>("he"), padReadIndexReg), addressSPad_14, _GEN_47) @[SPadModule.scala 142:12]
    node _GEN_49 = mux(eq(UInt<4>("hf"), padReadIndexReg), addressSPad_15, _GEN_48) @[SPadModule.scala 142:12]
    node _addressSPad_padReadIndexReg = _GEN_49 @[SPadModule.scala 142:12 SPadModule.scala 142:12 SPadModule.scala 142:12 SPadModule.scala 142:12 SPadModule.scala 142:12 SPadModule.scala 142:12 SPadModule.scala 142:12 SPadModule.scala 142:12 SPadModule.scala 142:12 SPadModule.scala 142:12 SPadModule.scala 142:12 SPadModule.scala 142:12 SPadModule.scala 142:12 SPadModule.scala 142:12 SPadModule.scala 142:12 SPadModule.scala 142:12 SPadModule.scala 142:12]
    node dataWire = _addressSPad_padReadIndexReg @[SPadModule.scala 182:38 SPadModule.scala 142:12]
    node _T_4 = eq(dataWire, UInt<1>("h0")) @[SPadModule.scala 191:28]
    node readIndexInc = io_ctrlPath_indexInc @[SPadModule.scala 187:42 SPadModule.scala 144:16]
    node _T_5 = and(_T_4, readIndexInc) @[SPadModule.scala 191:36]
    node _T_6 = add(padWriteIndexReg, UInt<1>("h1")) @[SPadModule.scala 194:79]
    node _T_7 = tail(_T_6, 1) @[SPadModule.scala 194:79]
    node writeWrapWire = _T_3 @[SPadModule.scala 185:43 SPadModule.scala 190:17]
    node _T_8 = mux(writeWrapWire, UInt<1>("h0"), _T_7) @[SPadModule.scala 194:41]
    node _T_9 = mux(writeFire, _T_8, padWriteIndexReg) @[SPadModule.scala 194:26]
    node _addressSPad_padWriteIndexReg = io_dataPath_writeInData_data_bits @[SPadModule.scala 137:31 SPadModule.scala 137:31]
    node _GEN_0 = mux(eq(UInt<1>("h0"), padWriteIndexReg), _addressSPad_padWriteIndexReg, addressSPad_0) @[SPadModule.scala 137:31]
    node _GEN_1 = mux(eq(UInt<1>("h1"), padWriteIndexReg), _addressSPad_padWriteIndexReg, addressSPad_1) @[SPadModule.scala 137:31]
    node _GEN_2 = mux(eq(UInt<2>("h2"), padWriteIndexReg), _addressSPad_padWriteIndexReg, addressSPad_2) @[SPadModule.scala 137:31]
    node _GEN_3 = mux(eq(UInt<2>("h3"), padWriteIndexReg), _addressSPad_padWriteIndexReg, addressSPad_3) @[SPadModule.scala 137:31]
    node _GEN_4 = mux(eq(UInt<3>("h4"), padWriteIndexReg), _addressSPad_padWriteIndexReg, addressSPad_4) @[SPadModule.scala 137:31]
    node _GEN_5 = mux(eq(UInt<3>("h5"), padWriteIndexReg), _addressSPad_padWriteIndexReg, addressSPad_5) @[SPadModule.scala 137:31]
    node _GEN_6 = mux(eq(UInt<3>("h6"), padWriteIndexReg), _addressSPad_padWriteIndexReg, addressSPad_6) @[SPadModule.scala 137:31]
    node _GEN_7 = mux(eq(UInt<3>("h7"), padWriteIndexReg), _addressSPad_padWriteIndexReg, addressSPad_7) @[SPadModule.scala 137:31]
    node _GEN_8 = mux(eq(UInt<4>("h8"), padWriteIndexReg), _addressSPad_padWriteIndexReg, addressSPad_8) @[SPadModule.scala 137:31]
    node _GEN_9 = mux(eq(UInt<4>("h9"), padWriteIndexReg), _addressSPad_padWriteIndexReg, addressSPad_9) @[SPadModule.scala 137:31]
    node _GEN_10 = mux(eq(UInt<4>("ha"), padWriteIndexReg), _addressSPad_padWriteIndexReg, addressSPad_10) @[SPadModule.scala 137:31]
    node _GEN_11 = mux(eq(UInt<4>("hb"), padWriteIndexReg), _addressSPad_padWriteIndexReg, addressSPad_11) @[SPadModule.scala 137:31]
    node _GEN_12 = mux(eq(UInt<4>("hc"), padWriteIndexReg), _addressSPad_padWriteIndexReg, addressSPad_12) @[SPadModule.scala 137:31]
    node _GEN_13 = mux(eq(UInt<4>("hd"), padWriteIndexReg), _addressSPad_padWriteIndexReg, addressSPad_13) @[SPadModule.scala 137:31]
    node _GEN_14 = mux(eq(UInt<4>("he"), padWriteIndexReg), _addressSPad_padWriteIndexReg, addressSPad_14) @[SPadModule.scala 137:31]
    node _GEN_15 = mux(eq(UInt<4>("hf"), padWriteIndexReg), _addressSPad_padWriteIndexReg, addressSPad_15) @[SPadModule.scala 137:31]
    node _GEN_16 = mux(writeFire, _GEN_0, addressSPad_0) @[SPadModule.scala 136:20]
    node _GEN_17 = mux(writeFire, _GEN_1, addressSPad_1) @[SPadModule.scala 136:20]
    node _GEN_18 = mux(writeFire, _GEN_2, addressSPad_2) @[SPadModule.scala 136:20]
    node _GEN_19 = mux(writeFire, _GEN_3, addressSPad_3) @[SPadModule.scala 136:20]
    node _GEN_20 = mux(writeFire, _GEN_4, addressSPad_4) @[SPadModule.scala 136:20]
    node _GEN_21 = mux(writeFire, _GEN_5, addressSPad_5) @[SPadModule.scala 136:20]
    node _GEN_22 = mux(writeFire, _GEN_6, addressSPad_6) @[SPadModule.scala 136:20]
    node _GEN_23 = mux(writeFire, _GEN_7, addressSPad_7) @[SPadModule.scala 136:20]
    node _GEN_24 = mux(writeFire, _GEN_8, addressSPad_8) @[SPadModule.scala 136:20]
    node _GEN_25 = mux(writeFire, _GEN_9, addressSPad_9) @[SPadModule.scala 136:20]
    node _GEN_26 = mux(writeFire, _GEN_10, addressSPad_10) @[SPadModule.scala 136:20]
    node _GEN_27 = mux(writeFire, _GEN_11, addressSPad_11) @[SPadModule.scala 136:20]
    node _GEN_28 = mux(writeFire, _GEN_12, addressSPad_12) @[SPadModule.scala 136:20]
    node _GEN_29 = mux(writeFire, _GEN_13, addressSPad_13) @[SPadModule.scala 136:20]
    node _GEN_30 = mux(writeFire, _GEN_14, addressSPad_14) @[SPadModule.scala 136:20]
    node _GEN_31 = mux(writeFire, _GEN_15, addressSPad_15) @[SPadModule.scala 136:20]
    node _T_11 = add(padReadIndexReg, UInt<1>("h1")) @[SPadModule.scala 201:42]
    node _T_12 = tail(_T_11, 1) @[SPadModule.scala 201:42]
    node readWrapWire = _T_5 @[SPadModule.scala 186:42 SPadModule.scala 191:16]
    node _GEN_32 = mux(readWrapWire, UInt<1>("h0"), _T_12) @[SPadModule.scala 202:27]
    node _GEN_33 = mux(readIndexInc, _GEN_32, padReadIndexReg) @[SPadModule.scala 200:25]
    node _GEN_50 = mux(io_ctrlPath_readInIdxEn, io_ctrlPath_readInIdx, _GEN_33) @[SPadModule.scala 124:34]
    node _T_10_0 = UInt<7>("h0") @[SPadModule.scala 133:53 SPadModule.scala 133:53]
    node _T_10_1 = UInt<7>("h0") @[SPadModule.scala 133:53 SPadModule.scala 133:53]
    node _T_10_2 = UInt<7>("h0") @[SPadModule.scala 133:53 SPadModule.scala 133:53]
    node _T_10_3 = UInt<7>("h0") @[SPadModule.scala 133:53 SPadModule.scala 133:53]
    node _T_10_4 = UInt<7>("h0") @[SPadModule.scala 133:53 SPadModule.scala 133:53]
    node _T_10_5 = UInt<7>("h0") @[SPadModule.scala 133:53 SPadModule.scala 133:53]
    node _T_10_6 = UInt<7>("h0") @[SPadModule.scala 133:53 SPadModule.scala 133:53]
    node _T_10_7 = UInt<7>("h0") @[SPadModule.scala 133:53 SPadModule.scala 133:53]
    node _T_10_8 = UInt<7>("h0") @[SPadModule.scala 133:53 SPadModule.scala 133:53]
    node _T_10_9 = UInt<7>("h0") @[SPadModule.scala 133:53 SPadModule.scala 133:53]
    node _T_10_10 = UInt<7>("h0") @[SPadModule.scala 133:53 SPadModule.scala 133:53]
    node _T_10_11 = UInt<7>("h0") @[SPadModule.scala 133:53 SPadModule.scala 133:53]
    node _T_10_12 = UInt<7>("h0") @[SPadModule.scala 133:53 SPadModule.scala 133:53]
    node _T_10_13 = UInt<7>("h0") @[SPadModule.scala 133:53 SPadModule.scala 133:53]
    node _T_10_14 = UInt<7>("h0") @[SPadModule.scala 133:53 SPadModule.scala 133:53]
    node _T_10_15 = UInt<7>("h0") @[SPadModule.scala 133:53 SPadModule.scala 133:53]
    io_dataPath_columnNum <= padReadIndexReg @[SPadModule.scala 198:25]
    io_dataPath_readOutData <= dataWire @[SPadModule.scala 143:27]
    io_dataPath_writeInData_data_ready <= UInt<1>("h1") @[SPadModule.scala 193:25]
    io_ctrlPath_writeIdx <= padWriteIndexReg @[SPadModule.scala 196:24]
    io_ctrlPath_writeFin <= writeWrapWire @[SPadModule.scala 197:24]
    padWriteIndexReg <= mux(reset, UInt<4>("h0"), _T_9) @[SPadModule.scala 194:20]
    padReadIndexReg <= mux(reset, UInt<4>("h0"), _GEN_50) @[SPadModule.scala 201:23 SPadModule.scala 203:25 SPadModule.scala 125:21]
    addressSPad_0 <= mux(reset, _T_10_0, _GEN_16) @[SPadModule.scala 137:31]
    addressSPad_1 <= mux(reset, _T_10_1, _GEN_17) @[SPadModule.scala 137:31]
    addressSPad_2 <= mux(reset, _T_10_2, _GEN_18) @[SPadModule.scala 137:31]
    addressSPad_3 <= mux(reset, _T_10_3, _GEN_19) @[SPadModule.scala 137:31]
    addressSPad_4 <= mux(reset, _T_10_4, _GEN_20) @[SPadModule.scala 137:31]
    addressSPad_5 <= mux(reset, _T_10_5, _GEN_21) @[SPadModule.scala 137:31]
    addressSPad_6 <= mux(reset, _T_10_6, _GEN_22) @[SPadModule.scala 137:31]
    addressSPad_7 <= mux(reset, _T_10_7, _GEN_23) @[SPadModule.scala 137:31]
    addressSPad_8 <= mux(reset, _T_10_8, _GEN_24) @[SPadModule.scala 137:31]
    addressSPad_9 <= mux(reset, _T_10_9, _GEN_25) @[SPadModule.scala 137:31]
    addressSPad_10 <= mux(reset, _T_10_10, _GEN_26) @[SPadModule.scala 137:31]
    addressSPad_11 <= mux(reset, _T_10_11, _GEN_27) @[SPadModule.scala 137:31]
    addressSPad_12 <= mux(reset, _T_10_12, _GEN_28) @[SPadModule.scala 137:31]
    addressSPad_13 <= mux(reset, _T_10_13, _GEN_29) @[SPadModule.scala 137:31]
    addressSPad_14 <= mux(reset, _T_10_14, _GEN_30) @[SPadModule.scala 137:31]
    addressSPad_15 <= mux(reset, _T_10_15, _GEN_31) @[SPadModule.scala 137:31]

  module SPadDataModule_1 :
    input clock : Clock
    input reset : UInt<1>
    output io_dataPath_columnNum : UInt<7>
    output io_dataPath_readOutData : UInt<12>
    output io_dataPath_writeInData_data_ready : UInt<1>
    input io_dataPath_writeInData_data_valid : UInt<1>
    input io_dataPath_writeInData_data_bits : UInt<12>
    input io_ctrlPath_writeEn : UInt<1>
    output io_ctrlPath_writeIdx : UInt<7>
    output io_ctrlPath_writeFin : UInt<1>
    input io_ctrlPath_readEn : UInt<1>
    input io_ctrlPath_readInIdx : UInt<7>
    input io_ctrlPath_indexInc : UInt<1>
    input io_ctrlPath_readInIdxEn : UInt<1>
  
    mem dataSPadSRAM : @[SPadModule.scala 150:50]
      data-type => UInt<12>
      depth => 96
      read-latency => 1
      write-latency => 1
      reader => _T_16
      writer => _T_10
      read-under-write => undefined
    reg padWriteIndexReg : UInt<7>, clock with :
      reset => (UInt<1>("h0"), padWriteIndexReg) @[SPadModule.scala 183:49]
    reg padReadIndexReg : UInt<7>, clock with :
      reset => (UInt<1>("h0"), padReadIndexReg) @[SPadModule.scala 184:48]
    node _T = and(io_dataPath_writeInData_data_ready, io_dataPath_writeInData_data_valid) @[Decoupled.scala 40:37]
    node _T_1 = and(_T, io_ctrlPath_writeEn) @[SPadModule.scala 189:39]
    node _T_2 = eq(io_dataPath_writeInData_data_bits, UInt<1>("h0")) @[SPadModule.scala 190:41]
    node writeFire = _T_1 @[SPadModule.scala 188:39 SPadModule.scala 189:13]
    node _T_3 = and(_T_2, writeFire) @[SPadModule.scala 190:49]
    node dataWire = dataSPadSRAM._T_16.data @[SPadModule.scala 182:38 SPadModule.scala 163:14]
    node _T_4 = eq(dataWire, UInt<1>("h0")) @[SPadModule.scala 191:28]
    node readIndexInc = io_ctrlPath_indexInc @[SPadModule.scala 187:42 SPadModule.scala 175:16]
    node _T_5 = and(_T_4, readIndexInc) @[SPadModule.scala 191:36]
    node _T_6 = add(padWriteIndexReg, UInt<1>("h1")) @[SPadModule.scala 194:79]
    node _T_7 = tail(_T_6, 1) @[SPadModule.scala 194:79]
    node writeWrapWire = _T_3 @[SPadModule.scala 185:43 SPadModule.scala 190:17]
    node _T_8 = mux(writeWrapWire, UInt<1>("h0"), _T_7) @[SPadModule.scala 194:41]
    node _T_9 = mux(writeFire, _T_8, padWriteIndexReg) @[SPadModule.scala 194:26]
    node _GEN_0 = validif(writeFire, padWriteIndexReg) @[SPadModule.scala 153:22]
    node _GEN_1 = validif(writeFire, clock) @[SPadModule.scala 153:22]
    node _GEN_2 = mux(writeFire, UInt<1>("h1"), UInt<1>("h0")) @[SPadModule.scala 153:22]
    node _GEN_3 = validif(writeFire, UInt<1>("h1")) @[SPadModule.scala 153:22]
    node _GEN_4 = validif(writeFire, io_dataPath_writeInData_data_bits) @[SPadModule.scala 153:22]
    node _T_11 = add(padReadIndexReg, UInt<1>("h1")) @[SPadModule.scala 201:42]
    node _T_12 = tail(_T_11, 1) @[SPadModule.scala 201:42]
    node readWrapWire = _T_5 @[SPadModule.scala 186:42 SPadModule.scala 191:16]
    node _GEN_5 = mux(readWrapWire, UInt<1>("h0"), _T_12) @[SPadModule.scala 202:27]
    node _GEN_6 = mux(readIndexInc, _GEN_5, padReadIndexReg) @[SPadModule.scala 200:25]
    node _GEN_7 = mux(io_ctrlPath_readInIdxEn, io_ctrlPath_readInIdx, _GEN_6) @[SPadModule.scala 157:36]
    node _GEN_8 = validif(io_ctrlPath_readEn, padReadIndexReg) @[SPadModule.scala 163:30]
    node _T_13 = _GEN_8 @[SPadModule.scala 163:30 SPadModule.scala 163:30]
    node _T_14 = or(_T_13, UInt<7>("h0")) @[SPadModule.scala 163:30]
    node _T_15 = bits(_T_14, 6, 0) @[SPadModule.scala 163:30]
    node _GEN_9 = mux(io_ctrlPath_readEn, UInt<1>("h1"), UInt<1>("h0")) @[SPadModule.scala 163:30]
    node _GEN_10 = validif(io_ctrlPath_readEn, _T_15) @[SPadModule.scala 163:30]
    node _GEN_11 = validif(io_ctrlPath_readEn, clock) @[SPadModule.scala 163:30]
    io_dataPath_columnNum <= padReadIndexReg @[SPadModule.scala 198:25]
    io_dataPath_readOutData <= dataWire @[SPadModule.scala 176:27]
    io_dataPath_writeInData_data_ready <= UInt<1>("h1") @[SPadModule.scala 193:25]
    io_ctrlPath_writeIdx <= padWriteIndexReg @[SPadModule.scala 196:24]
    io_ctrlPath_writeFin <= writeWrapWire @[SPadModule.scala 197:24]
    padWriteIndexReg <= mux(reset, UInt<7>("h0"), _T_9) @[SPadModule.scala 194:20]
    padReadIndexReg <= mux(reset, UInt<7>("h0"), _GEN_7) @[SPadModule.scala 158:23 SPadModule.scala 201:23 SPadModule.scala 203:25]
    dataSPadSRAM._T_16.addr <= _GEN_10 @[SPadModule.scala 163:30]
    dataSPadSRAM._T_16.en <= _GEN_9 @[SPadModule.scala 150:50 SPadModule.scala 163:30]
    dataSPadSRAM._T_16.clk <= _GEN_11 @[SPadModule.scala 163:30]
    dataSPadSRAM._T_10.addr <= _GEN_0
    dataSPadSRAM._T_10.en <= _GEN_2 @[SPadModule.scala 150:50]
    dataSPadSRAM._T_10.clk <= _GEN_1
    dataSPadSRAM._T_10.data <= _GEN_4
    dataSPadSRAM._T_10.mask <= _GEN_3

  module nonToeplitzProcessingElementPad :
    input clock : Clock
    input reset : UInt<1>
    input io_padCtrl_doMACEn : UInt<1>
    input io_padCtrl_fromTopIO_pSumEnqEn : UInt<1>
    input io_padCtrl_fromTopIO_doLoadEn : UInt<1>
    output io_padCtrl_fromTopIO_writeFinish : UInt<1>
    output io_padCtrl_fromTopIO_calFinish : UInt<1>
    output io_dataStream_ipsIO_ready : UInt<1>
    input io_dataStream_ipsIO_valid : UInt<1>
    input io_dataStream_ipsIO_bits : UInt<21>
    input io_dataStream_opsIO_ready : UInt<1>
    output io_dataStream_opsIO_valid : UInt<1>
    output io_dataStream_opsIO_bits : UInt<21>
    output io_dataStream_inActIOs_adrIOs_data_ready : UInt<1>
    input io_dataStream_inActIOs_adrIOs_data_valid : UInt<1>
    input io_dataStream_inActIOs_adrIOs_data_bits : UInt<7>
    output io_dataStream_inActIOs_dataIOs_data_ready : UInt<1>
    input io_dataStream_inActIOs_dataIOs_data_valid : UInt<1>
    input io_dataStream_inActIOs_dataIOs_data_bits : UInt<12>
    output io_dataStream_weightIOs_adrIOs_data_ready : UInt<1>
    input io_dataStream_weightIOs_adrIOs_data_valid : UInt<1>
    input io_dataStream_weightIOs_adrIOs_data_bits : UInt<7>
    output io_dataStream_weightIOs_dataIOs_data_ready : UInt<1>
    input io_dataStream_weightIOs_dataIOs_data_valid : UInt<1>
    input io_dataStream_weightIOs_dataIOs_data_bits : UInt<12>
    output io_debugIO_inActMatrixData : UInt<8>
    output io_debugIO_inActMatrixRow : UInt<4>
    output io_debugIO_inActMatrixColumn : UInt<7>
    output io_debugIO_inActAdrInc : UInt<1>
    output io_debugIO_inActDataInc : UInt<1>
    output io_debugIO_inActAdrIdx : UInt<7>
    output io_debugIO_weightAdrSPadReadOut : UInt<7>
    output io_debugIO_weightMatrixData : UInt<8>
    output io_debugIO_weightMatrixRow : UInt<4>
    output io_debugIO_productResult : UInt<21>
    output io_debugIO_pSumResult : UInt<21>
    output io_debugIO_pSumLoad : UInt<21>
    output io_debugIO_weightAdrInIdx : UInt<4>
    output io_debugIO_sPadState : UInt<3>
    output io_debugIO_pSumReadIdx : UInt<5>
    output io_debugIO_pSumPadReadIdx : UInt<5>
    output io_debugIO_inActDataSliding : UInt<1>
    output io_debugIO_inActDataSlidingFire : UInt<1>
    output io_debugIO_futureLBStart : UInt<4>
    output io_debugIO_inActDataIndex : UInt<6>
    output io_debugIO_inActAdrData : UInt<7>
    output io_padWF_inActWriteFin_adrWriteFin : UInt<1>
    output io_padWF_inActWriteFin_dataWriteFin : UInt<1>
    output io_padWF_weightWriteFin_adrWriteFin : UInt<1>
    output io_padWF_weightWriteFin_dataWriteFin : UInt<1>
    output io_padWF_pSumAddFin : UInt<1>
  
    inst pSumSPadModule of PSumSPad @[nonToeplitzProcessingElement.scala 122:44]
    inst inActAdrSPadModule of SPadDataModule @[nonToeplitzProcessingElement.scala 124:54]
    inst inActDataSPadModule of nonTpActSPadDataModule @[nonToeplitzProcessingElement.scala 126:63]
    inst weightAdrSPadModule of WeightSPadAdrModule @[nonToeplitzProcessingElement.scala 128:60]
    inst weightDataSPadModule of SPadDataModule_1 @[nonToeplitzProcessingElement.scala 130:56]
    reg inActSPadZeroColumnReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), inActSPadZeroColumnReg) @[nonToeplitzProcessingElement.scala 141:55]
    reg inActAdrSPadReadEnReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), inActAdrSPadReadEnReg) @[nonToeplitzProcessingElement.scala 156:54]
    reg inActDataSPadReadEnReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), inActDataSPadReadEnReg) @[nonToeplitzProcessingElement.scala 157:55]
    reg inActMatrixColumnReg : UInt<4>, clock with :
      reset => (UInt<1>("h0"), inActMatrixColumnReg) @[nonToeplitzProcessingElement.scala 160:53]
    reg inActZeroColumnNumber : UInt<4>, clock with :
      reset => (UInt<1>("h0"), inActZeroColumnNumber) @[nonToeplitzProcessingElement.scala 161:54]
    reg inActFirstRead : UInt<1>, clock with :
      reset => (UInt<1>("h0"), inActFirstRead) @[nonToeplitzProcessingElement.scala 162:58]
    reg weightAdrSPadReadEnReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), weightAdrSPadReadEnReg) @[nonToeplitzProcessingElement.scala 172:55]
    reg weightDataSPadReadEnReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), weightDataSPadReadEnReg) @[nonToeplitzProcessingElement.scala 173:56]
    reg weightDataSPadFirstRead : UInt<1>, clock with :
      reset => (UInt<1>("h0"), weightDataSPadFirstRead) @[nonToeplitzProcessingElement.scala 177:56]
    reg productReg : UInt<21>, clock with :
      reset => (UInt<1>("h0"), productReg) @[nonToeplitzProcessingElement.scala 184:43]
    reg pSumSPadLoadReg : UInt<21>, clock with :
      reset => (UInt<1>("h0"), pSumSPadLoadReg) @[nonToeplitzProcessingElement.scala 185:48]
    reg PESPadStateReg : UInt<3>, clock with :
      reset => (UInt<1>("h0"), PESPadStateReg) @[nonToeplitzProcessingElement.scala 197:37]
    node _T = eq(PESPadStateReg, UInt<3>("h1")) @[nonToeplitzProcessingElement.scala 204:19]
    node _T_1 = eq(PESPadStateReg, UInt<3>("h6")) @[nonToeplitzProcessingElement.scala 205:20]
    node _T_2 = eq(PESPadStateReg, UInt<3>("h7")) @[nonToeplitzProcessingElement.scala 206:19]
    node _T_3 = eq(PESPadStateReg, UInt<3>("h3")) @[nonToeplitzProcessingElement.scala 207:19]
    node _T_4 = eq(PESPadStateReg, UInt<3>("h2")) @[nonToeplitzProcessingElement.scala 208:19]
    node _T_21 = bits(inActDataSPadModule.io_dataPath_readOutData, 3, 0) @[nonToeplitzProcessingElement.scala 280:61]
    reg currentSliding : UInt<1>, clock with :
      reset => (UInt<1>("h0"), currentSliding) @[nonToeplitzProcessingElement.scala 229:47]
    node _T_22 = mul(UInt<2>("h2"), currentSliding) @[nonToeplitzProcessingElement.scala 280:91]
    node _T_23 = sub(_T_21, _T_22) @[nonToeplitzProcessingElement.scala 280:84]
    node _T_24 = tail(_T_23, 1) @[nonToeplitzProcessingElement.scala 280:84]
    node inActMatrixRowWire = _T_24 @[nonToeplitzProcessingElement.scala 164:48 nonToeplitzProcessingElement.scala 280:22]
    node _T_5 = eq(inActMatrixRowWire, UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 209:53]
    reg currentLowerBound : UInt<4>, clock with :
      reset => (UInt<1>("h0"), currentLowerBound) @[nonToeplitzProcessingElement.scala 234:50]
    reg currentUpperBound : UInt<4>, clock with :
      reset => (UInt<1>("h0"), currentUpperBound) @[nonToeplitzProcessingElement.scala 235:50]
    node _T_6 = mul(currentSliding, UInt<3>("h4")) @[nonToeplitzProcessingElement.scala 248:60]
    node _T_26 = bits(weightDataSPadModule.io_dataPath_readOutData, 3, 0) @[nonToeplitzProcessingElement.scala 304:62]
    node weightMatrixRowReg = _T_26 @[nonToeplitzProcessingElement.scala 210:48 nonToeplitzProcessingElement.scala 304:22]
    node _T_7 = add(weightMatrixRowReg, _T_6) @[nonToeplitzProcessingElement.scala 248:43]
    node _T_8 = tail(_T_7, 1) @[nonToeplitzProcessingElement.scala 248:43]
    node _T_9 = mul(inActMatrixColumnReg, UInt<2>("h2")) @[nonToeplitzProcessingElement.scala 248:90]
    node _T_10 = mul(_T_9, UInt<3>("h4")) @[nonToeplitzProcessingElement.scala 248:96]
    node _T_11 = add(_T_8, _T_10) @[nonToeplitzProcessingElement.scala 248:67]
    node _T_12 = tail(_T_11, 1) @[nonToeplitzProcessingElement.scala 248:67]
    node inActDataIndexWire = inActDataSPadModule.io_dataPath_columnNum @[nonToeplitzProcessingElement.scala 155:48 nonToeplitzProcessingElement.scala 278:22]
    node _T_13 = add(inActDataIndexWire, UInt<1>("h1")) @[nonToeplitzProcessingElement.scala 249:69]
    node _T_14 = tail(_T_13, 1) @[nonToeplitzProcessingElement.scala 249:69]
    node inActAdrData = inActAdrSPadModule.io_dataPath_readOutData @[nonToeplitzProcessingElement.scala 153:46 nonToeplitzProcessingElement.scala 269:20]
    node _T_15 = eq(inActAdrData, _T_14) @[nonToeplitzProcessingElement.scala 249:45]
    node _T_16 = sub(UInt<2>("h2"), UInt<1>("h1")) @[nonToeplitzProcessingElement.scala 250:30]
    node _T_17 = tail(_T_16, 1) @[nonToeplitzProcessingElement.scala 250:30]
    node _T_18 = eq(currentSliding, _T_17) @[nonToeplitzProcessingElement.scala 250:21]
    node _T_19 = and(_T_15, _T_18) @[nonToeplitzProcessingElement.scala 249:77]
    node _T_20 = bits(inActDataSPadModule.io_dataPath_readOutData, 11, 4) @[nonToeplitzProcessingElement.scala 279:62]
    node _T_25 = bits(weightDataSPadModule.io_dataPath_readOutData, 11, 4) @[nonToeplitzProcessingElement.scala 303:63]
    node weightMatrixReadFirstColumn = _T_5 @[nonToeplitzProcessingElement.scala 182:57 nonToeplitzProcessingElement.scala 209:31]
    node weightAdrDataWire = weightAdrSPadModule.io_dataPath_readOutData @[nonToeplitzProcessingElement.scala 170:47 nonToeplitzProcessingElement.scala 291:21]
    node _T_27 = mux(weightMatrixReadFirstColumn, UInt<1>("h0"), weightAdrDataWire) @[nonToeplitzProcessingElement.scala 307:45]
    reg pSumLoadStateReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), pSumLoadStateReg) @[nonToeplitzProcessingElement.scala 312:49]
    node pSumSPadLoadWire = pSumSPadModule.io_dataPath_opsIO_bits @[nonToeplitzProcessingElement.scala 186:46 nonToeplitzProcessingElement.scala 346:20]
    node _T_28 = mux(pSumLoadStateReg, pSumSPadLoadWire, pSumSPadLoadReg) @[nonToeplitzProcessingElement.scala 314:46]
    node _T_29 = mux(pSumLoadStateReg, io_dataStream_ipsIO_bits, productReg) @[nonToeplitzProcessingElement.scala 315:8]
    node _T_30 = add(_T_28, _T_29) @[nonToeplitzProcessingElement.scala 314:100]
    node pSumAddResultWire = tail(_T_30, 1) @[nonToeplitzProcessingElement.scala 314:100]
    node padEqWB = _T_2 @[nonToeplitzProcessingElement.scala 203:37 nonToeplitzProcessingElement.scala 206:11]
    node pSumResultWire = mux(padEqWB, pSumAddResultWire, UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 316:43]
    reg pSumPadReadIdxReg : UInt<5>, clock with :
      reset => (UInt<1>("h0"), pSumPadReadIdxReg) @[nonToeplitzProcessingElement.scala 317:50]
    node _T_31 = mux(io_padCtrl_fromTopIO_pSumEnqEn, UInt<1>("h1"), pSumLoadStateReg) @[nonToeplitzProcessingElement.scala 324:8]
    node _T_32 = mux(io_padWF_pSumAddFin, UInt<1>("h0"), _T_31) @[nonToeplitzProcessingElement.scala 323:26]
    node pSumReadOutValid = and(io_dataStream_ipsIO_valid, pSumLoadStateReg) @[nonToeplitzProcessingElement.scala 325:68]
    node padEqID = _T_4 @[nonToeplitzProcessingElement.scala 200:37 nonToeplitzProcessingElement.scala 208:11]
    node inActMatrixDataWire = _T_20 @[nonToeplitzProcessingElement.scala 166:49 nonToeplitzProcessingElement.scala 279:23]
    node _T_61 = eq(inActMatrixDataWire, UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 367:47]
    node _T_62 = eq(inActFirstRead, UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 367:58]
    node _T_63 = and(_T_61, _T_62) @[nonToeplitzProcessingElement.scala 367:55]
    node mightInActReadFinish = _T_63 @[nonToeplitzProcessingElement.scala 145:50 nonToeplitzProcessingElement.scala 367:24]
    node _T_45 = and(padEqID, mightInActReadFinish) @[nonToeplitzProcessingElement.scala 359:31]
    node _T_46 = and(padEqWB, mightInActReadFinish) @[nonToeplitzProcessingElement.scala 359:68]
    node _T_47 = or(_T_45, _T_46) @[nonToeplitzProcessingElement.scala 359:56]
    node _T_35 = and(io_dataStream_opsIO_ready, io_dataStream_opsIO_valid) @[Decoupled.scala 40:37]
    node pSumPadReadIdxIncWire = _T_35 @[nonToeplitzProcessingElement.scala 319:51 nonToeplitzProcessingElement.scala 331:25]
    node _T_48 = sub(UInt<2>("h2"), UInt<1>("h1")) @[nonToeplitzProcessingElement.scala 360:54]
    node _T_49 = tail(_T_48, 1) @[nonToeplitzProcessingElement.scala 360:54]
    node _T_50 = eq(currentSliding, _T_49) @[nonToeplitzProcessingElement.scala 360:46]
    node _T_51 = and(pSumPadReadIdxIncWire, _T_50) @[nonToeplitzProcessingElement.scala 360:28]
    node _T_52 = eq(pSumPadReadIdxReg, UInt<5>("h17")) @[nonToeplitzProcessingElement.scala 360:81]
    node _T_53 = and(_T_51, _T_52) @[nonToeplitzProcessingElement.scala 360:60]
    node _T_54 = or(_T_47, _T_53) @[nonToeplitzProcessingElement.scala 359:93]
    node pSumPadReadWrap = _T_54 @[nonToeplitzProcessingElement.scala 321:45 nonToeplitzProcessingElement.scala 359:19]
    node _T_33 = and(pSumPadReadWrap, pSumLoadStateReg) @[nonToeplitzProcessingElement.scala 328:42]
    node _T_34 = and(io_dataStream_opsIO_ready, pSumLoadStateReg) @[nonToeplitzProcessingElement.scala 329:58]
    node _T_36 = and(pSumSPadModule.io_dataPath_opsIO_ready, pSumSPadModule.io_dataPath_opsIO_valid) @[Decoupled.scala 40:37]
    node _T_37 = and(_T_36, pSumReadOutValid) @[nonToeplitzProcessingElement.scala 338:65]
    node padEqMpy = _T_1 @[nonToeplitzProcessingElement.scala 202:38 nonToeplitzProcessingElement.scala 205:12]
    node _T_38 = or(padEqMpy, pSumReadOutValid) @[nonToeplitzProcessingElement.scala 349:47]
    node _T_39 = eq(PESPadStateReg, UInt<3>("h5")) @[nonToeplitzProcessingElement.scala 354:33]
    node _T_40 = add(pSumPadReadIdxReg, UInt<1>("h1")) @[nonToeplitzProcessingElement.scala 355:76]
    node _T_41 = tail(_T_40, 1) @[nonToeplitzProcessingElement.scala 355:76]
    node _T_42 = mux(pSumPadReadIdxIncWire, _T_41, pSumPadReadIdxReg) @[nonToeplitzProcessingElement.scala 355:34]
    node _T_43 = mux(pSumPadReadWrap, UInt<1>("h0"), _T_42) @[nonToeplitzProcessingElement.scala 355:8]
    node psDataSPadIdxWire = bits(_T_12, 4, 0) @[nonToeplitzProcessingElement.scala 151:47 nonToeplitzProcessingElement.scala 248:21]
    node _T_44 = mux(_T_39, psDataSPadIdxWire, _T_43) @[nonToeplitzProcessingElement.scala 354:27]
    node _T_55 = eq(inActAdrData, UInt<7>("h7f")) @[nonToeplitzProcessingElement.scala 363:48]
    node _T_56 = eq(weightAdrDataWire, UInt<7>("h7f")) @[nonToeplitzProcessingElement.scala 364:50]
    node weightDataIndexWire = weightDataSPadModule.io_dataPath_columnNum @[nonToeplitzProcessingElement.scala 171:49 nonToeplitzProcessingElement.scala 302:23]
    node _T_57 = add(weightDataIndexWire, UInt<1>("h1")) @[nonToeplitzProcessingElement.scala 366:71]
    node _T_58 = tail(_T_57, 1) @[nonToeplitzProcessingElement.scala 366:71]
    node _T_59 = eq(weightAdrDataWire, _T_58) @[nonToeplitzProcessingElement.scala 366:46]
    node weightMatrixDataReg = _T_25 @[nonToeplitzProcessingElement.scala 176:49 nonToeplitzProcessingElement.scala 303:23]
    node _T_64 = eq(weightMatrixDataReg, UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 369:48]
    node _T_65 = eq(weightDataSPadFirstRead, UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 369:59]
    node _T_66 = and(_T_64, _T_65) @[nonToeplitzProcessingElement.scala 369:56]
    node mightWeightReadFinish = _T_66 @[nonToeplitzProcessingElement.scala 148:51 nonToeplitzProcessingElement.scala 369:25]
    node _T_60 = or(_T_59, mightWeightReadFinish) @[nonToeplitzProcessingElement.scala 366:78]
    node _T_67 = eq(weightAdrDataWire, UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 370:48]
    node padEqIA = _T @[nonToeplitzProcessingElement.scala 199:37 nonToeplitzProcessingElement.scala 204:11]
    node mightInActZeroColumnWire = _T_55 @[nonToeplitzProcessingElement.scala 140:54 nonToeplitzProcessingElement.scala 363:28]
    node _T_68 = and(padEqIA, mightInActZeroColumnWire) @[nonToeplitzProcessingElement.scala 371:38]
    node padEqWA = _T_3 @[nonToeplitzProcessingElement.scala 201:37 nonToeplitzProcessingElement.scala 207:11]
    node mightWeightZeroColumnWire = _T_56 @[nonToeplitzProcessingElement.scala 143:55 nonToeplitzProcessingElement.scala 364:29]
    node _T_69 = and(padEqWA, mightWeightZeroColumnWire) @[nonToeplitzProcessingElement.scala 371:81]
    node mightWeightIdxIncWire = _T_60 @[nonToeplitzProcessingElement.scala 144:51 nonToeplitzProcessingElement.scala 366:25]
    node _T_70 = and(padEqWB, mightWeightIdxIncWire) @[nonToeplitzProcessingElement.scala 372:14]
    node _T_71 = or(_T_69, _T_70) @[nonToeplitzProcessingElement.scala 371:111]
    node mightInActIdxIncWire = _T_19 @[nonToeplitzProcessingElement.scala 142:50 nonToeplitzProcessingElement.scala 249:24]
    node _T_72 = and(_T_71, mightInActIdxIncWire) @[nonToeplitzProcessingElement.scala 372:41]
    node _T_73 = or(_T_68, _T_72) @[nonToeplitzProcessingElement.scala 371:67]
    node _T_74 = neq(PESPadStateReg, UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 372:99]
    node _T_75 = and(mightInActReadFinish, _T_74) @[nonToeplitzProcessingElement.scala 372:91]
    node _T_76 = or(_T_73, _T_75) @[nonToeplitzProcessingElement.scala 372:66]
    node _T_77 = eq(PESPadStateReg, UInt<3>("h4")) @[nonToeplitzProcessingElement.scala 373:48]
    node _T_78 = or(padEqMpy, _T_77) @[nonToeplitzProcessingElement.scala 373:40]
    node _T_79 = and(_T_78, mightWeightZeroColumnWire) @[nonToeplitzProcessingElement.scala 373:68]
    node _T_80 = eq(mightInActZeroColumnWire, UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 375:42]
    node _T_81 = and(padEqIA, _T_80) @[nonToeplitzProcessingElement.scala 375:39]
    node _T_82 = eq(inActFirstRead, UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 375:71]
    node _T_83 = and(_T_81, _T_82) @[nonToeplitzProcessingElement.scala 375:68]
    node _T_84 = and(padEqWA, mightWeightZeroColumnWire) @[nonToeplitzProcessingElement.scala 376:16]
    node _T_85 = and(padEqWB, mightWeightIdxIncWire) @[nonToeplitzProcessingElement.scala 377:16]
    node _T_86 = or(_T_84, _T_85) @[nonToeplitzProcessingElement.scala 376:46]
    node _T_87 = eq(mightInActIdxIncWire, UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 377:46]
    node _T_88 = and(_T_86, _T_87) @[nonToeplitzProcessingElement.scala 377:43]
    node _T_89 = or(_T_83, _T_88) @[nonToeplitzProcessingElement.scala 375:99]
    node _T_90 = neq(PESPadStateReg, UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 378:35]
    node _T_91 = and(mightInActReadFinish, _T_90) @[nonToeplitzProcessingElement.scala 378:27]
    node _T_92 = or(_T_89, _T_91) @[nonToeplitzProcessingElement.scala 377:69]
    node mightInActReadIdxBeyondUB = inActDataSPadModule.io_ctrlPath_inActReadIdxBeyondUB @[nonToeplitzProcessingElement.scala 224:55 nonToeplitzProcessingElement.scala 242:29]
    node _T_93 = and(padEqID, mightInActReadIdxBeyondUB) @[nonToeplitzProcessingElement.scala 378:56]
    node _T_94 = or(_T_92, _T_93) @[nonToeplitzProcessingElement.scala 378:44]
    node _T_95 = eq(mightWeightZeroColumnWire, UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 379:43]
    node _T_96 = and(padEqWA, _T_95) @[nonToeplitzProcessingElement.scala 379:40]
    node _T_97 = eq(weightDataSPadFirstRead, UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 379:73]
    node _T_98 = and(_T_96, _T_97) @[nonToeplitzProcessingElement.scala 379:70]
    node _T_99 = eq(mightWeightIdxIncWire, UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 380:17]
    node _T_100 = and(padEqWB, _T_99) @[nonToeplitzProcessingElement.scala 380:14]
    node _T_101 = or(_T_98, _T_100) @[nonToeplitzProcessingElement.scala 379:99]
    node _T_102 = or(padEqID, padEqWA) @[nonToeplitzProcessingElement.scala 381:34]
    node _T_103 = and(_T_102, weightDataSPadFirstRead) @[nonToeplitzProcessingElement.scala 381:46]
    node _T_104 = and(padEqID, weightDataSPadFirstRead) @[nonToeplitzProcessingElement.scala 383:35]
    node _T_105 = eq(weightMatrixReadFirstColumn, UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 383:65]
    node _T_106 = and(_T_104, _T_105) @[nonToeplitzProcessingElement.scala 383:62]
    node _T_107 = sub(inActMatrixRowWire, UInt<1>("h1")) @[nonToeplitzProcessingElement.scala 384:76]
    node _T_108 = tail(_T_107, 1) @[nonToeplitzProcessingElement.scala 384:76]
    node weightDataIdxMuxWire = _T_106 @[nonToeplitzProcessingElement.scala 178:50 nonToeplitzProcessingElement.scala 383:24]
    node _T_109 = mux(weightDataIdxMuxWire, _T_108, inActMatrixRowWire) @[nonToeplitzProcessingElement.scala 384:34]
    node _T_110 = and(padEqWA, weightDataSPadFirstRead) @[nonToeplitzProcessingElement.scala 385:34]
    node _T_111 = eq(mightWeightZeroColumnWire, UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 385:64]
    node _T_112 = and(_T_110, _T_111) @[nonToeplitzProcessingElement.scala 385:61]
    node _T_113 = neq(PESPadStateReg, UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 386:66]
    node _T_114 = and(mightInActReadFinish, _T_113) @[nonToeplitzProcessingElement.scala 386:58]
    node _T_115 = eq(UInt<3>("h0"), PESPadStateReg) @[Conditional.scala 37:30]
    node _GEN_0 = mux(io_padCtrl_doMACEn, UInt<3>("h1"), PESPadStateReg) @[nonToeplitzProcessingElement.scala 390:32]
    node _GEN_1 = mux(io_padCtrl_doMACEn, UInt<1>("h1"), inActAdrSPadReadEnReg) @[nonToeplitzProcessingElement.scala 390:32]
    node _GEN_2 = mux(io_padCtrl_doMACEn, UInt<1>("h0"), inActDataSPadReadEnReg) @[nonToeplitzProcessingElement.scala 390:32]
    node _GEN_3 = mux(io_padCtrl_doMACEn, UInt<1>("h0"), weightAdrSPadReadEnReg) @[nonToeplitzProcessingElement.scala 390:32]
    node _GEN_4 = mux(io_padCtrl_doMACEn, UInt<1>("h0"), weightDataSPadReadEnReg) @[nonToeplitzProcessingElement.scala 390:32]
    node _GEN_5 = mux(io_padCtrl_doMACEn, UInt<1>("h1"), weightDataSPadFirstRead) @[nonToeplitzProcessingElement.scala 390:32]
    node _T_116 = eq(UInt<3>("h1"), PESPadStateReg) @[Conditional.scala 37:30]
    node _T_117 = add(inActZeroColumnNumber, UInt<1>("h1")) @[nonToeplitzProcessingElement.scala 398:56]
    node _T_118 = tail(_T_117, 1) @[nonToeplitzProcessingElement.scala 398:56]
    node _GEN_6 = mux(mightInActZeroColumnWire, UInt<3>("h1"), UInt<3>("h2")) @[nonToeplitzProcessingElement.scala 395:38]
    node _GEN_7 = mux(mightInActZeroColumnWire, UInt<1>("h1"), UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 395:38]
    node _GEN_8 = mux(mightInActZeroColumnWire, UInt<1>("h0"), UInt<1>("h1")) @[nonToeplitzProcessingElement.scala 395:38]
    node _GEN_9 = mux(mightInActZeroColumnWire, UInt<1>("h0"), UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 395:38]
    node _GEN_10 = mux(mightInActZeroColumnWire, UInt<1>("h1"), UInt<1>("h1")) @[nonToeplitzProcessingElement.scala 395:38]
    node _GEN_11 = mux(mightInActZeroColumnWire, UInt<1>("h1"), inActSPadZeroColumnReg) @[nonToeplitzProcessingElement.scala 395:38]
    node _GEN_12 = mux(mightInActZeroColumnWire, _T_118, inActZeroColumnNumber) @[nonToeplitzProcessingElement.scala 395:38]
    node _T_119 = eq(UInt<3>("h2"), PESPadStateReg) @[Conditional.scala 37:30]
    node _T_120 = add(currentSliding, UInt<1>("h1")) @[nonToeplitzProcessingElement.scala 408:42]
    node _T_121 = tail(_T_120, 1) @[nonToeplitzProcessingElement.scala 408:42]
    node mightInActSlidingIncWire = inActDataSPadModule.io_ctrlPath_slidingInc @[nonToeplitzProcessingElement.scala 223:54 nonToeplitzProcessingElement.scala 241:28]
    node _GEN_13 = mux(mightInActSlidingIncWire, UInt<3>("h2"), UInt<3>("h3")) @[nonToeplitzProcessingElement.scala 406:45]
    node _GEN_14 = mux(mightInActSlidingIncWire, UInt<1>("h0"), UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 406:45]
    node _GEN_15 = mux(mightInActSlidingIncWire, UInt<1>("h1"), UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 406:45]
    node _GEN_16 = mux(mightInActSlidingIncWire, UInt<1>("h0"), UInt<1>("h1")) @[nonToeplitzProcessingElement.scala 406:45]
    node _GEN_17 = mux(mightInActSlidingIncWire, UInt<1>("h1"), weightDataSPadFirstRead) @[nonToeplitzProcessingElement.scala 406:45]
    node _GEN_18 = mux(mightInActSlidingIncWire, _T_121, currentSliding) @[nonToeplitzProcessingElement.scala 406:45]
    node _GEN_19 = mux(mightInActReadFinish, UInt<3>("h0"), _GEN_13) @[nonToeplitzProcessingElement.scala 404:34]
    node _GEN_20 = mux(mightInActReadFinish, UInt<1>("h1"), inActFirstRead) @[nonToeplitzProcessingElement.scala 404:34]
    node _GEN_21 = mux(mightInActReadFinish, UInt<1>("h0"), inActMatrixColumnReg) @[nonToeplitzProcessingElement.scala 404:34]
    node _GEN_22 = mux(mightInActReadFinish, inActAdrSPadReadEnReg, _GEN_14) @[nonToeplitzProcessingElement.scala 404:34]
    node _GEN_23 = mux(mightInActReadFinish, inActDataSPadReadEnReg, _GEN_15) @[nonToeplitzProcessingElement.scala 404:34]
    node _GEN_24 = mux(mightInActReadFinish, weightAdrSPadReadEnReg, _GEN_16) @[nonToeplitzProcessingElement.scala 404:34]
    node _GEN_25 = mux(mightInActReadFinish, weightDataSPadReadEnReg, _GEN_14) @[nonToeplitzProcessingElement.scala 404:34]
    node _GEN_26 = mux(mightInActReadFinish, weightDataSPadFirstRead, _GEN_17) @[nonToeplitzProcessingElement.scala 404:34]
    node _GEN_27 = mux(mightInActReadFinish, currentSliding, _GEN_18) @[nonToeplitzProcessingElement.scala 404:34]
    node _T_122 = eq(UInt<3>("h3"), PESPadStateReg) @[Conditional.scala 37:30]
    node _T_123 = add(inActMatrixColumnReg, UInt<1>("h1")) @[nonToeplitzProcessingElement.scala 421:58]
    node _T_124 = tail(_T_123, 1) @[nonToeplitzProcessingElement.scala 421:58]
    node _T_125 = add(_T_124, inActZeroColumnNumber) @[nonToeplitzProcessingElement.scala 421:64]
    node _T_126 = tail(_T_125, 1) @[nonToeplitzProcessingElement.scala 421:64]
    node _T_127 = add(inActMatrixColumnReg, UInt<1>("h1")) @[nonToeplitzProcessingElement.scala 424:58]
    node _T_128 = tail(_T_127, 1) @[nonToeplitzProcessingElement.scala 424:58]
    node _GEN_28 = mux(inActSPadZeroColumnReg, UInt<1>("h0"), inActSPadZeroColumnReg) @[nonToeplitzProcessingElement.scala 419:40]
    node _GEN_29 = mux(inActSPadZeroColumnReg, _T_126, _T_128) @[nonToeplitzProcessingElement.scala 419:40]
    node _GEN_30 = mux(inActSPadZeroColumnReg, UInt<1>("h0"), inActZeroColumnNumber) @[nonToeplitzProcessingElement.scala 419:40]
    node _GEN_31 = mux(mightInActIdxIncWire, UInt<3>("h1"), UInt<3>("h2")) @[nonToeplitzProcessingElement.scala 416:36]
    node _GEN_32 = mux(mightInActIdxIncWire, UInt<1>("h1"), UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 416:36]
    node _GEN_33 = mux(mightInActIdxIncWire, UInt<1>("h0"), UInt<1>("h1")) @[nonToeplitzProcessingElement.scala 416:36]
    node _GEN_34 = mux(mightInActIdxIncWire, UInt<1>("h0"), UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 416:36]
    node _GEN_35 = mux(mightInActIdxIncWire, UInt<1>("h1"), UInt<1>("h1")) @[nonToeplitzProcessingElement.scala 416:36]
    node _GEN_36 = mux(mightInActIdxIncWire, UInt<1>("h0"), currentSliding) @[nonToeplitzProcessingElement.scala 416:36]
    node _GEN_37 = mux(mightInActIdxIncWire, _GEN_28, inActSPadZeroColumnReg) @[nonToeplitzProcessingElement.scala 416:36]
    node _GEN_38 = mux(mightInActIdxIncWire, _GEN_29, inActMatrixColumnReg) @[nonToeplitzProcessingElement.scala 416:36]
    node _GEN_39 = mux(mightInActIdxIncWire, _GEN_30, inActZeroColumnNumber) @[nonToeplitzProcessingElement.scala 416:36]
    node _GEN_40 = mux(mightWeightZeroColumnWire, _GEN_31, UInt<3>("h4")) @[nonToeplitzProcessingElement.scala 415:39]
    node _GEN_41 = mux(mightWeightZeroColumnWire, _GEN_32, UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 415:39]
    node _GEN_42 = mux(mightWeightZeroColumnWire, _GEN_33, UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 415:39]
    node _GEN_43 = mux(mightWeightZeroColumnWire, _GEN_34, UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 415:39]
    node _GEN_44 = mux(mightWeightZeroColumnWire, _GEN_34, UInt<1>("h1")) @[nonToeplitzProcessingElement.scala 415:39]
    node _GEN_45 = mux(mightWeightZeroColumnWire, _GEN_35, weightDataSPadFirstRead) @[nonToeplitzProcessingElement.scala 415:39]
    node _GEN_46 = mux(mightWeightZeroColumnWire, _GEN_36, currentSliding) @[nonToeplitzProcessingElement.scala 415:39]
    node _GEN_47 = mux(mightWeightZeroColumnWire, _GEN_37, inActSPadZeroColumnReg) @[nonToeplitzProcessingElement.scala 415:39]
    node _GEN_48 = mux(mightWeightZeroColumnWire, _GEN_38, inActMatrixColumnReg) @[nonToeplitzProcessingElement.scala 415:39]
    node _GEN_49 = mux(mightWeightZeroColumnWire, _GEN_39, inActZeroColumnNumber) @[nonToeplitzProcessingElement.scala 415:39]
    node _T_129 = eq(UInt<3>("h4"), PESPadStateReg) @[Conditional.scala 37:30]
    node _T_130 = eq(UInt<3>("h5"), PESPadStateReg) @[Conditional.scala 37:30]
    node _T_131 = eq(UInt<3>("h6"), PESPadStateReg) @[Conditional.scala 37:30]
    node _T_132 = mul(weightMatrixDataReg, inActMatrixDataWire) @[nonToeplitzProcessingElement.scala 442:41]
    node _T_133 = eq(UInt<3>("h7"), PESPadStateReg) @[Conditional.scala 37:30]
    node _T_134 = add(currentSliding, UInt<1>("h1")) @[nonToeplitzProcessingElement.scala 455:46]
    node _T_135 = tail(_T_134, 1) @[nonToeplitzProcessingElement.scala 455:46]
    node _T_136 = add(inActMatrixColumnReg, UInt<1>("h1")) @[nonToeplitzProcessingElement.scala 462:60]
    node _T_137 = tail(_T_136, 1) @[nonToeplitzProcessingElement.scala 462:60]
    node _T_138 = add(_T_137, inActZeroColumnNumber) @[nonToeplitzProcessingElement.scala 462:66]
    node _T_139 = tail(_T_138, 1) @[nonToeplitzProcessingElement.scala 462:66]
    node _T_140 = add(inActMatrixColumnReg, UInt<1>("h1")) @[nonToeplitzProcessingElement.scala 465:60]
    node _T_141 = tail(_T_140, 1) @[nonToeplitzProcessingElement.scala 465:60]
    node _GEN_50 = mux(inActSPadZeroColumnReg, _T_139, _T_141) @[nonToeplitzProcessingElement.scala 460:42]
    node _GEN_51 = mux(mightInActIdxIncWire, _GEN_50, inActMatrixColumnReg) @[nonToeplitzProcessingElement.scala 457:44]
    node _GEN_52 = mux(mightInActSlidingIncWire, _T_135, _GEN_36) @[nonToeplitzProcessingElement.scala 454:42]
    node _GEN_53 = mux(mightInActSlidingIncWire, UInt<3>("h2"), _GEN_31) @[nonToeplitzProcessingElement.scala 454:42]
    node _GEN_54 = mux(mightInActSlidingIncWire, UInt<1>("h0"), _GEN_32) @[nonToeplitzProcessingElement.scala 454:42]
    node _GEN_55 = mux(mightInActSlidingIncWire, UInt<1>("h1"), _GEN_33) @[nonToeplitzProcessingElement.scala 454:42]
    node _GEN_56 = mux(mightInActSlidingIncWire, UInt<1>("h0"), _GEN_34) @[nonToeplitzProcessingElement.scala 454:42]
    node _GEN_57 = mux(mightInActSlidingIncWire, UInt<1>("h1"), _GEN_35) @[nonToeplitzProcessingElement.scala 454:42]
    node _GEN_58 = mux(mightInActSlidingIncWire, inActSPadZeroColumnReg, _GEN_37) @[nonToeplitzProcessingElement.scala 454:42]
    node _GEN_59 = mux(mightInActSlidingIncWire, inActMatrixColumnReg, _GEN_51) @[nonToeplitzProcessingElement.scala 454:42]
    node _GEN_60 = mux(mightInActSlidingIncWire, inActZeroColumnNumber, _GEN_39) @[nonToeplitzProcessingElement.scala 454:42]
    node _GEN_61 = mux(mightWeightIdxIncWire, _GEN_52, currentSliding) @[nonToeplitzProcessingElement.scala 452:37]
    node _GEN_62 = mux(mightWeightIdxIncWire, _GEN_53, UInt<3>("h4")) @[nonToeplitzProcessingElement.scala 452:37]
    node _GEN_63 = mux(mightWeightIdxIncWire, _GEN_54, UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 452:37]
    node _GEN_64 = mux(mightWeightIdxIncWire, _GEN_55, UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 452:37]
    node _GEN_65 = mux(mightWeightIdxIncWire, _GEN_56, UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 452:37]
    node _GEN_66 = mux(mightWeightIdxIncWire, _GEN_56, UInt<1>("h1")) @[nonToeplitzProcessingElement.scala 452:37]
    node _GEN_67 = mux(mightWeightIdxIncWire, _GEN_57, UInt<1>("h0")) @[nonToeplitzProcessingElement.scala 452:37]
    node _GEN_68 = mux(mightWeightIdxIncWire, _GEN_58, inActSPadZeroColumnReg) @[nonToeplitzProcessingElement.scala 452:37]
    node _GEN_69 = mux(mightWeightIdxIncWire, _GEN_59, inActMatrixColumnReg) @[nonToeplitzProcessingElement.scala 452:37]
    node _GEN_70 = mux(mightWeightIdxIncWire, _GEN_60, inActZeroColumnNumber) @[nonToeplitzProcessingElement.scala 452:37]
    node _GEN_71 = mux(mightInActReadFinish, UInt<3>("h0"), _GEN_62) @[nonToeplitzProcessingElement.scala 449:34]
    node _GEN_72 = mux(mightInActReadFinish, UInt<1>("h0"), _GEN_69) @[nonToeplitzProcessingElement.scala 449:34]
    node _GEN_73 = mux(mightInActReadFinish, currentSliding, _GEN_61) @[nonToeplitzProcessingElement.scala 449:34]
    node _GEN_74 = mux(mightInActReadFinish, inActAdrSPadReadEnReg, _GEN_63) @[nonToeplitzProcessingElement.scala 449:34]
    node _GEN_75 = mux(mightInActReadFinish, inActDataSPadReadEnReg, _GEN_64) @[nonToeplitzProcessingElement.scala 449:34]
    node _GEN_76 = mux(mightInActReadFinish, weightAdrSPadReadEnReg, _GEN_65) @[nonToeplitzProcessingElement.scala 449:34]
    node _GEN_77 = mux(mightInActReadFinish, weightDataSPadReadEnReg, _GEN_66) @[nonToeplitzProcessingElement.scala 449:34]
    node _GEN_78 = mux(mightInActReadFinish, weightDataSPadFirstRead, _GEN_67) @[nonToeplitzProcessingElement.scala 449:34]
    node _GEN_79 = mux(mightInActReadFinish, inActSPadZeroColumnReg, _GEN_68) @[nonToeplitzProcessingElement.scala 449:34]
    node _GEN_80 = mux(mightInActReadFinish, inActZeroColumnNumber, _GEN_70) @[nonToeplitzProcessingElement.scala 449:34]
    node _GEN_81 = mux(_T_133, _GEN_71, PESPadStateReg) @[Conditional.scala 39:67]
    node _GEN_82 = mux(_T_133, _GEN_20, inActFirstRead) @[Conditional.scala 39:67]
    node _GEN_83 = mux(_T_133, _GEN_72, inActMatrixColumnReg) @[Conditional.scala 39:67]
    node _GEN_84 = mux(_T_133, _GEN_73, currentSliding) @[Conditional.scala 39:67]
    node _GEN_85 = mux(_T_133, _GEN_74, inActAdrSPadReadEnReg) @[Conditional.scala 39:67]
    node _GEN_86 = mux(_T_133, _GEN_75, inActDataSPadReadEnReg) @[Conditional.scala 39:67]
    node _GEN_87 = mux(_T_133, _GEN_76, weightAdrSPadReadEnReg) @[Conditional.scala 39:67]
    node _GEN_88 = mux(_T_133, _GEN_77, weightDataSPadReadEnReg) @[Conditional.scala 39:67]
    node _GEN_89 = mux(_T_133, _GEN_78, weightDataSPadFirstRead) @[Conditional.scala 39:67]
    node _GEN_90 = mux(_T_133, _GEN_79, inActSPadZeroColumnReg) @[Conditional.scala 39:67]
    node _GEN_91 = mux(_T_133, _GEN_80, inActZeroColumnNumber) @[Conditional.scala 39:67]
    node _GEN_92 = mux(_T_131, UInt<3>("h7"), _GEN_81) @[Conditional.scala 39:67]
    node _GEN_93 = mux(_T_131, _T_132, productReg) @[Conditional.scala 39:67]
    node _GEN_94 = mux(_T_131, inActFirstRead, _GEN_82) @[Conditional.scala 39:67]
    node _GEN_95 = mux(_T_131, inActMatrixColumnReg, _GEN_83) @[Conditional.scala 39:67]
    node _GEN_96 = mux(_T_131, currentSliding, _GEN_84) @[Conditional.scala 39:67]
    node _GEN_97 = mux(_T_131, inActAdrSPadReadEnReg, _GEN_85) @[Conditional.scala 39:67]
    node _GEN_98 = mux(_T_131, inActDataSPadReadEnReg, _GEN_86) @[Conditional.scala 39:67]
    node _GEN_99 = mux(_T_131, weightAdrSPadReadEnReg, _GEN_87) @[Conditional.scala 39:67]
    node _GEN_100 = mux(_T_131, weightDataSPadReadEnReg, _GEN_88) @[Conditional.scala 39:67]
    node _GEN_101 = mux(_T_131, weightDataSPadFirstRead, _GEN_89) @[Conditional.scala 39:67]
    node _GEN_102 = mux(_T_131, inActSPadZeroColumnReg, _GEN_90) @[Conditional.scala 39:67]
    node _GEN_103 = mux(_T_131, inActZeroColumnNumber, _GEN_91) @[Conditional.scala 39:67]
    node _GEN_104 = mux(_T_130, UInt<3>("h6"), _GEN_92) @[Conditional.scala 39:67]
    node _GEN_105 = mux(_T_130, UInt<1>("h0"), _GEN_97) @[Conditional.scala 39:67]
    node _GEN_106 = mux(_T_130, UInt<1>("h0"), _GEN_98) @[Conditional.scala 39:67]
    node _GEN_107 = mux(_T_130, UInt<1>("h0"), _GEN_99) @[Conditional.scala 39:67]
    node _GEN_108 = mux(_T_130, UInt<1>("h0"), _GEN_100) @[Conditional.scala 39:67]
    node _GEN_109 = mux(_T_130, productReg, _GEN_93) @[Conditional.scala 39:67]
    node _GEN_110 = mux(_T_130, inActFirstRead, _GEN_94) @[Conditional.scala 39:67]
    node _GEN_111 = mux(_T_130, inActMatrixColumnReg, _GEN_95) @[Conditional.scala 39:67]
    node _GEN_112 = mux(_T_130, currentSliding, _GEN_96) @[Conditional.scala 39:67]
    node _GEN_113 = mux(_T_130, weightDataSPadFirstRead, _GEN_101) @[Conditional.scala 39:67]
    node _GEN_114 = mux(_T_130, inActSPadZeroColumnReg, _GEN_102) @[Conditional.scala 39:67]
    node _GEN_115 = mux(_T_130, inActZeroColumnNumber, _GEN_103) @[Conditional.scala 39:67]
    node _GEN_116 = mux(_T_129, UInt<3>("h5"), _GEN_104) @[Conditional.scala 39:67]
    node _GEN_117 = mux(_T_129, inActAdrSPadReadEnReg, _GEN_105) @[Conditional.scala 39:67]
    node _GEN_118 = mux(_T_129, inActDataSPadReadEnReg, _GEN_106) @[Conditional.scala 39:67]
    node _GEN_119 = mux(_T_129, weightAdrSPadReadEnReg, _GEN_107) @[Conditional.scala 39:67]
    node _GEN_120 = mux(_T_129, weightDataSPadReadEnReg, _GEN_108) @[Conditional.scala 39:67]
    node _GEN_121 = mux(_T_129, productReg, _GEN_109) @[Conditional.scala 39:67]
    node _GEN_122 = mux(_T_129, inActFirstRead, _GEN_110) @[Conditional.scala 39:67]
    node _GEN_123 = mux(_T_129, inActMatrixColumnReg, _GEN_111) @[Conditional.scala 39:67]
    node _GEN_124 = mux(_T_129, currentSliding, _GEN_112) @[Conditional.scala 39:67]
    node _GEN_125 = mux(_T_129, weightDataSPadFirstRead, _GEN_113) @[Conditional.scala 39:67]
    node _GEN_126 = mux(_T_129, inActSPadZeroColumnReg, _GEN_114) @[Conditional.scala 39:67]
    node _GEN_127 = mux(_T_129, inActZeroColumnNumber, _GEN_115) @[Conditional.scala 39:67]
    node _GEN_128 = mux(_T_122, UInt<1>("h0"), _GEN_122) @[Conditional.scala 39:67]
    node _GEN_129 = mux(_T_122, _GEN_40, _GEN_116) @[Conditional.scala 39:67]
    node _GEN_130 = mux(_T_122, _GEN_41, _GEN_117) @[Conditional.scala 39:67]
    node _GEN_131 = mux(_T_122, _GEN_42, _GEN_118) @[Conditional.scala 39:67]
    node _GEN_132 = mux(_T_122, _GEN_43, _GEN_119) @[Conditional.scala 39:67]
    node _GEN_133 = mux(_T_122, _GEN_44, _GEN_120) @[Conditional.scala 39:67]
    node _GEN_134 = mux(_T_122, _GEN_45, _GEN_125) @[Conditional.scala 39:67]
    node _GEN_135 = mux(_T_122, _GEN_46, _GEN_124) @[Conditional.scala 39:67]
    node _GEN_136 = mux(_T_122, _GEN_47, _GEN_126) @[Conditional.scala 39:67]
    node _GEN_137 = mux(_T_122, _GEN_48, _GEN_123) @[Conditional.scala 39:67]
    node _GEN_138 = mux(_T_122, _GEN_49, _GEN_127) @[Conditional.scala 39:67]
    node _GEN_139 = mux(_T_122, productReg, _GEN_121) @[Conditional.scala 39:67]
    node _GEN_140 = mux(_T_119, _GEN_19, _GEN_129) @[Conditional.scala 39:67]
    node _GEN_141 = mux(_T_119, _GEN_20, _GEN_128) @[Conditional.scala 39:67]
    node _GEN_142 = mux(_T_119, _GEN_21, _GEN_137) @[Conditional.scala 39:67]
    node _GEN_143 = mux(_T_119, _GEN_22, _GEN_130) @[Conditional.scala 39:67]
    node _GEN_144 = mux(_T_119, _GEN_23, _GEN_131) @[Conditional.scala 39:67]
    node _GEN_145 = mux(_T_119, _GEN_24, _GEN_132) @[Conditional.scala 39:67]
    node _GEN_146 = mux(_T_119, _GEN_25, _GEN_133) @[Conditional.scala 39:67]
    node _GEN_147 = mux(_T_119, _GEN_26, _GEN_134) @[Conditional.scala 39:67]
    node _GEN_148 = mux(_T_119, _GEN_27, _GEN_135) @[Conditional.scala 39:67]
    node _GEN_149 = mux(_T_119, inActSPadZeroColumnReg, _GEN_136) @[Conditional.scala 39:67]
    node _GEN_150 = mux(_T_119, inActZeroColumnNumber, _GEN_138) @[Conditional.scala 39:67]
    node _GEN_151 = mux(_T_119, productReg, _GEN_139) @[Conditional.scala 39:67]
    node _GEN_152 = mux(_T_116, _GEN_6, _GEN_140) @[Conditional.scala 39:67]
    node _GEN_153 = mux(_T_116, _GEN_7, _GEN_143) @[Conditional.scala 39:67]
    node _GEN_154 = mux(_T_116, _GEN_8, _GEN_144) @[Conditional.scala 39:67]
    node _GEN_155 = mux(_T_116, _GEN_9, _GEN_145) @[Conditional.scala 39:67]
    node _GEN_156 = mux(_T_116, _GEN_9, _GEN_146) @[Conditional.scala 39:67]
    node _GEN_157 = mux(_T_116, _GEN_10, _GEN_147) @[Conditional.scala 39:67]
    node _GEN_158 = mux(_T_116, _GEN_11, _GEN_149) @[Conditional.scala 39:67]
    node _GEN_159 = mux(_T_116, _GEN_12, _GEN_150) @[Conditional.scala 39:67]
    node _GEN_160 = mux(_T_116, inActFirstRead, _GEN_141) @[Conditional.scala 39:67]
    node _GEN_161 = mux(_T_116, inActMatrixColumnReg, _GEN_142) @[Conditional.scala 39:67]
    node _GEN_162 = mux(_T_116, currentSliding, _GEN_148) @[Conditional.scala 39:67]
    node _GEN_163 = mux(_T_116, productReg, _GEN_151) @[Conditional.scala 39:67]
    node _GEN_164 = mux(_T_115, _GEN_0, _GEN_152) @[Conditional.scala 40:58]
    node _GEN_165 = mux(_T_115, _GEN_1, _GEN_153) @[Conditional.scala 40:58]
    node _GEN_166 = mux(_T_115, _GEN_2, _GEN_154) @[Conditional.scala 40:58]
    node _GEN_167 = mux(_T_115, _GEN_3, _GEN_155) @[Conditional.scala 40:58]
    node _GEN_168 = mux(_T_115, _GEN_4, _GEN_156) @[Conditional.scala 40:58]
    node _GEN_169 = mux(_T_115, _GEN_5, _GEN_157) @[Conditional.scala 40:58]
    node _GEN_170 = mux(_T_115, inActSPadZeroColumnReg, _GEN_158) @[Conditional.scala 40:58]
    node _GEN_171 = mux(_T_115, inActZeroColumnNumber, _GEN_159) @[Conditional.scala 40:58]
    node _GEN_172 = mux(_T_115, inActFirstRead, _GEN_160) @[Conditional.scala 40:58]
    node _GEN_173 = mux(_T_115, inActMatrixColumnReg, _GEN_161) @[Conditional.scala 40:58]
    node _GEN_174 = mux(_T_115, currentSliding, _GEN_162) @[Conditional.scala 40:58]
    node _GEN_175 = mux(_T_115, productReg, _GEN_163) @[Conditional.scala 40:58]
    node mightWeightMatrixFinish = _T_67 @[nonToeplitzProcessingElement.scala 150:53 nonToeplitzProcessingElement.scala 370:27]
    node inActAdrIndexWire = inActAdrSPadModule.io_dataPath_columnNum @[nonToeplitzProcessingElement.scala 152:47 nonToeplitzProcessingElement.scala 268:21]
    node inActAdrSPadIdxIncWire = _T_76 @[nonToeplitzProcessingElement.scala 158:52 nonToeplitzProcessingElement.scala 371:26]
    node inActDataSPadIdxIncWire = _T_94 @[nonToeplitzProcessingElement.scala 159:53 nonToeplitzProcessingElement.scala 375:27]
    node weightAdrIndexWire = weightAdrSPadModule.io_dataPath_columnNum @[nonToeplitzProcessingElement.scala 169:48 nonToeplitzProcessingElement.scala 290:22]
    node weightAdrSPadIdxIncWire = _T_79 @[nonToeplitzProcessingElement.scala 174:53 nonToeplitzProcessingElement.scala 373:27]
    node weightDataSPadIdxIncWire = _T_101 @[nonToeplitzProcessingElement.scala 175:54 nonToeplitzProcessingElement.scala 379:28]
    node weightAdrSPadReadIdxWire = _T_109 @[nonToeplitzProcessingElement.scala 179:54 nonToeplitzProcessingElement.scala 384:28]
    node weightDataIdxEnWire = _T_112 @[nonToeplitzProcessingElement.scala 180:49 nonToeplitzProcessingElement.scala 385:23]
    node weightAdrIdxEnWire = _T_103 @[nonToeplitzProcessingElement.scala 181:48 nonToeplitzProcessingElement.scala 381:22]
    node mightInActLastNonZeroEle = inActDataSPadModule.io_ctrlPath_inActLastNonZeroEle @[nonToeplitzProcessingElement.scala 225:54 nonToeplitzProcessingElement.scala 243:28]
    io_padCtrl_fromTopIO_writeFinish is invalid
    io_padCtrl_fromTopIO_calFinish <= _T_114 @[nonToeplitzProcessingElement.scala 386:34]
    io_dataStream_ipsIO_ready <= _T_34 @[nonToeplitzProcessingElement.scala 329:29]
    io_dataStream_opsIO_valid <= _T_37 @[nonToeplitzProcessingElement.scala 338:29]
    io_dataStream_opsIO_bits <= pSumAddResultWire @[nonToeplitzProcessingElement.scala 334:28]
    io_dataStream_inActIOs_adrIOs_data_ready <= inActAdrSPadModule.io_dataPath_writeInData_data_ready @[nonToeplitzProcessingElement.scala 267:39]
    io_dataStream_inActIOs_dataIOs_data_ready <= inActDataSPadModule.io_dataPath_writeInData_data_ready @[nonToeplitzProcessingElement.scala 277:40]
    io_dataStream_weightIOs_adrIOs_data_ready <= weightAdrSPadModule.io_dataPath_writeInData_data_ready @[nonToeplitzProcessingElement.scala 289:40]
    io_dataStream_weightIOs_dataIOs_data_ready <= weightDataSPadModule.io_dataPath_writeInData_data_ready @[nonToeplitzProcessingElement.scala 301:41]
    io_debugIO_inActMatrixData <= inActMatrixDataWire @[nonToeplitzProcessingElement.scala 480:32]
    io_debugIO_inActMatrixRow <= inActMatrixRowWire @[nonToeplitzProcessingElement.scala 481:31]
    io_debugIO_inActMatrixColumn <= inActMatrixColumnReg @[nonToeplitzProcessingElement.scala 479:34]
    io_debugIO_inActAdrInc <= inActAdrSPadIdxIncWire @[nonToeplitzProcessingElement.scala 483:28]
    io_debugIO_inActDataInc <= inActDataSPadIdxIncWire @[nonToeplitzProcessingElement.scala 484:29]
    io_debugIO_inActAdrIdx <= inActAdrIndexWire @[nonToeplitzProcessingElement.scala 482:28]
    io_debugIO_weightAdrSPadReadOut <= weightAdrDataWire @[nonToeplitzProcessingElement.scala 487:37]
    io_debugIO_weightMatrixData <= weightMatrixDataReg @[nonToeplitzProcessingElement.scala 485:33]
    io_debugIO_weightMatrixRow <= weightMatrixRowReg @[nonToeplitzProcessingElement.scala 486:32]
    io_debugIO_productResult <= productReg @[nonToeplitzProcessingElement.scala 488:30]
    io_debugIO_pSumResult <= pSumResultWire @[nonToeplitzProcessingElement.scala 489:27]
    io_debugIO_pSumLoad <= pSumSPadLoadReg @[nonToeplitzProcessingElement.scala 490:25]
    io_debugIO_weightAdrInIdx <= weightAdrSPadReadIdxWire @[nonToeplitzProcessingElement.scala 491:31]
    io_debugIO_sPadState <= PESPadStateReg @[nonToeplitzProcessingElement.scala 492:26]
    io_debugIO_pSumReadIdx <= pSumPadReadIdxReg @[nonToeplitzProcessingElement.scala 493:28]
    io_debugIO_pSumPadReadIdx <= pSumPadReadIdxReg @[nonToeplitzProcessingElement.scala 494:31]
    io_debugIO_inActDataSliding <= currentSliding @[nonToeplitzProcessingElement.scala 495:33]
    io_debugIO_inActDataSlidingFire <= mightInActSlidingIncWire @[nonToeplitzProcessingElement.scala 496:37]
    io_debugIO_futureLBStart <= inActDataSPadModule.io_dataPath_futureLBStart @[nonToeplitzProcessingElement.scala 497:30]
    io_debugIO_inActDataIndex <= inActDataIndexWire @[nonToeplitzProcessingElement.scala 498:31]
    io_debugIO_inActAdrData <= inActAdrData @[nonToeplitzProcessingElement.scala 499:29]
    io_padWF_inActWriteFin_adrWriteFin <= inActAdrSPadModule.io_ctrlPath_writeFin @[nonToeplitzProcessingElement.scala 270:38]
    io_padWF_inActWriteFin_dataWriteFin <= inActDataSPadModule.io_ctrlPath_writeFin @[nonToeplitzProcessingElement.scala 282:39]
    io_padWF_weightWriteFin_adrWriteFin <= weightAdrSPadModule.io_ctrlPath_writeFin @[nonToeplitzProcessingElement.scala 292:39]
    io_padWF_weightWriteFin_dataWriteFin <= weightDataSPadModule.io_ctrlPath_writeFin @[nonToeplitzProcessingElement.scala 305:40]
    io_padWF_pSumAddFin <= _T_33 @[nonToeplitzProcessingElement.scala 328:23]
    pSumSPadModule.clock <= clock
    pSumSPadModule.reset <= reset
    pSumSPadModule.io_dataPath_ipsIO_valid <= padEqWB @[nonToeplitzProcessingElement.scala 344:35]
    pSumSPadModule.io_dataPath_ipsIO_bits <= pSumResultWire @[nonToeplitzProcessingElement.scala 345:34]
    pSumSPadModule.io_dataPath_opsIO_ready <= _T_38 @[nonToeplitzProcessingElement.scala 349:35]
    pSumSPadModule.io_ctrlPath_readIdx <= pSumPadReadIdxReg @[nonToeplitzProcessingElement.scala 350:31]
    pSumSPadModule.io_ctrlPath_writeIdx <= psDataSPadIdxWire @[nonToeplitzProcessingElement.scala 351:32]
    inActAdrSPadModule.clock <= clock
    inActAdrSPadModule.reset <= reset
    inActAdrSPadModule.io_dataPath_writeInData_data_valid <= io_dataStream_inActIOs_adrIOs_data_valid @[nonToeplitzProcessingElement.scala 267:39]
    inActAdrSPadModule.io_dataPath_writeInData_data_bits <= io_dataStream_inActIOs_adrIOs_data_bits @[nonToeplitzProcessingElement.scala 267:39]
    inActAdrSPadModule.io_ctrlPath_writeEn <= io_padCtrl_fromTopIO_doLoadEn @[nonToeplitzProcessingElement.scala 215:35]
    inActAdrSPadModule.io_ctrlPath_readEn <= inActAdrSPadReadEnReg @[nonToeplitzProcessingElement.scala 271:34]
    inActAdrSPadModule.io_ctrlPath_readInIdx is invalid
    inActAdrSPadModule.io_ctrlPath_indexInc <= inActAdrSPadIdxIncWire @[nonToeplitzProcessingElement.scala 273:36]
    inActAdrSPadModule.io_ctrlPath_readInIdxEn is invalid
    inActDataSPadModule.clock <= clock
    inActDataSPadModule.reset <= reset
    inActDataSPadModule.io_dataPath_writeInData_data_valid <= io_dataStream_inActIOs_dataIOs_data_valid @[nonToeplitzProcessingElement.scala 277:40]
    inActDataSPadModule.io_dataPath_writeInData_data_bits <= io_dataStream_inActIOs_dataIOs_data_bits @[nonToeplitzProcessingElement.scala 277:40]
    inActDataSPadModule.io_dataPath_currentSliding <= currentSliding @[nonToeplitzProcessingElement.scala 237:43]
    inActDataSPadModule.io_ctrlPath_writeEn <= io_padCtrl_fromTopIO_doLoadEn @[nonToeplitzProcessingElement.scala 216:36]
    inActDataSPadModule.io_ctrlPath_readEn <= inActDataSPadReadEnReg @[nonToeplitzProcessingElement.scala 283:35]
    inActDataSPadModule.io_ctrlPath_readInIdx <= bits(inActAdrData, 5, 0) @[nonToeplitzProcessingElement.scala 284:38]
    inActDataSPadModule.io_ctrlPath_indexInc <= inActDataSPadIdxIncWire @[nonToeplitzProcessingElement.scala 285:37]
    inActDataSPadModule.io_ctrlPath_readInIdxEn is invalid
    inActDataSPadModule.io_ctrlPath_inActColInc <= mightInActIdxIncWire @[nonToeplitzProcessingElement.scala 251:40]
    inActDataSPadModule.io_ctrlPath_weightIdxInc <= mightWeightIdxIncWire @[nonToeplitzProcessingElement.scala 252:41]
    inActDataSPadModule.io_ctrlPath_padEqMpyBool <= padEqMpy @[nonToeplitzProcessingElement.scala 253:41]
    inActDataSPadModule.io_ctrlPath_mightWeightZeroColumn <= mightWeightZeroColumnWire @[nonToeplitzProcessingElement.scala 256:50]
    inActDataSPadModule.io_ctrlPath_padEqWABool <= padEqWA @[nonToeplitzProcessingElement.scala 254:40]
    inActDataSPadModule.io_ctrlPath_padEqIDBool <= padEqID @[nonToeplitzProcessingElement.scala 255:40]
    weightAdrSPadModule.clock <= clock
    weightAdrSPadModule.reset <= reset
    weightAdrSPadModule.io_dataPath_writeInData_data_valid <= io_dataStream_weightIOs_adrIOs_data_valid @[nonToeplitzProcessingElement.scala 289:40]
    weightAdrSPadModule.io_dataPath_writeInData_data_bits <= io_dataStream_weightIOs_adrIOs_data_bits @[nonToeplitzProcessingElement.scala 289:40]
    weightAdrSPadModule.io_ctrlPath_writeEn <= io_padCtrl_fromTopIO_doLoadEn @[nonToeplitzProcessingElement.scala 217:36]
    weightAdrSPadModule.io_ctrlPath_readEn <= weightAdrSPadReadEnReg @[nonToeplitzProcessingElement.scala 293:35]
    weightAdrSPadModule.io_ctrlPath_readInIdx <= weightAdrSPadReadIdxWire @[nonToeplitzProcessingElement.scala 294:38]
    weightAdrSPadModule.io_ctrlPath_indexInc <= weightAdrSPadIdxIncWire @[nonToeplitzProcessingElement.scala 297:37]
    weightAdrSPadModule.io_ctrlPath_readInIdxEn <= weightAdrIdxEnWire @[nonToeplitzProcessingElement.scala 298:40]
    weightDataSPadModule.clock <= clock
    weightDataSPadModule.reset <= reset
    weightDataSPadModule.io_dataPath_writeInData_data_valid <= io_dataStream_weightIOs_dataIOs_data_valid @[nonToeplitzProcessingElement.scala 301:41]
    weightDataSPadModule.io_dataPath_writeInData_data_bits <= io_dataStream_weightIOs_dataIOs_data_bits @[nonToeplitzProcessingElement.scala 301:41]
    weightDataSPadModule.io_ctrlPath_writeEn <= io_padCtrl_fromTopIO_doLoadEn @[nonToeplitzProcessingElement.scala 218:37]
    weightDataSPadModule.io_ctrlPath_readEn <= inActDataSPadReadEnReg @[nonToeplitzProcessingElement.scala 306:36]
    weightDataSPadModule.io_ctrlPath_readInIdx <= _T_27 @[nonToeplitzProcessingElement.scala 307:39]
    weightDataSPadModule.io_ctrlPath_indexInc <= weightDataSPadIdxIncWire @[nonToeplitzProcessingElement.scala 308:38]
    weightDataSPadModule.io_ctrlPath_readInIdxEn <= weightDataIdxEnWire @[nonToeplitzProcessingElement.scala 309:41]
    inActSPadZeroColumnReg <= mux(reset, UInt<1>("h0"), _GEN_170) @[nonToeplitzProcessingElement.scala 397:32 nonToeplitzProcessingElement.scala 420:36 nonToeplitzProcessingElement.scala 461:38]
    inActAdrSPadReadEnReg <= mux(reset, UInt<1>("h0"), _GEN_165) @[nonToeplitzProcessingElement.scala 76:27 nonToeplitzProcessingElement.scala 76:27 nonToeplitzProcessingElement.scala 85:27 nonToeplitzProcessingElement.scala 85:27 nonToeplitzProcessingElement.scala 94:27 nonToeplitzProcessingElement.scala 76:27 nonToeplitzProcessingElement.scala 85:27 nonToeplitzProcessingElement.scala 102:27 nonToeplitzProcessingElement.scala 109:27 nonToeplitzProcessingElement.scala 85:27 nonToeplitzProcessingElement.scala 76:27 nonToeplitzProcessingElement.scala 85:27 nonToeplitzProcessingElement.scala 102:27]
    inActDataSPadReadEnReg <= mux(reset, UInt<1>("h0"), _GEN_166) @[nonToeplitzProcessingElement.scala 77:28 nonToeplitzProcessingElement.scala 77:28 nonToeplitzProcessingElement.scala 86:28 nonToeplitzProcessingElement.scala 86:28 nonToeplitzProcessingElement.scala 95:28 nonToeplitzProcessingElement.scala 77:28 nonToeplitzProcessingElement.scala 86:28 nonToeplitzProcessingElement.scala 103:28 nonToeplitzProcessingElement.scala 110:28 nonToeplitzProcessingElement.scala 86:28 nonToeplitzProcessingElement.scala 77:28 nonToeplitzProcessingElement.scala 86:28 nonToeplitzProcessingElement.scala 103:28]
    inActMatrixColumnReg <= mux(reset, UInt<4>("h0"), _GEN_173) @[nonToeplitzProcessingElement.scala 118:26 nonToeplitzProcessingElement.scala 421:34 nonToeplitzProcessingElement.scala 424:34 nonToeplitzProcessingElement.scala 118:26 nonToeplitzProcessingElement.scala 462:36 nonToeplitzProcessingElement.scala 465:36]
    inActZeroColumnNumber <= mux(reset, UInt<4>("h0"), _GEN_171) @[nonToeplitzProcessingElement.scala 398:31 nonToeplitzProcessingElement.scala 422:35 nonToeplitzProcessingElement.scala 463:37]
    inActFirstRead <= mux(reset, UInt<1>("h1"), _GEN_172) @[nonToeplitzProcessingElement.scala 117:31 nonToeplitzProcessingElement.scala 414:33 nonToeplitzProcessingElement.scala 117:31]
    weightAdrSPadReadEnReg <= mux(reset, UInt<1>("h0"), _GEN_167) @[nonToeplitzProcessingElement.scala 78:28 nonToeplitzProcessingElement.scala 78:28 nonToeplitzProcessingElement.scala 87:28 nonToeplitzProcessingElement.scala 87:28 nonToeplitzProcessingElement.scala 96:28 nonToeplitzProcessingElement.scala 78:28 nonToeplitzProcessingElement.scala 87:28 nonToeplitzProcessingElement.scala 104:28 nonToeplitzProcessingElement.scala 111:28 nonToeplitzProcessingElement.scala 87:28 nonToeplitzProcessingElement.scala 78:28 nonToeplitzProcessingElement.scala 87:28 nonToeplitzProcessingElement.scala 104:28]
    weightDataSPadReadEnReg <= mux(reset, UInt<1>("h0"), _GEN_168) @[nonToeplitzProcessingElement.scala 79:29 nonToeplitzProcessingElement.scala 79:29 nonToeplitzProcessingElement.scala 88:29 nonToeplitzProcessingElement.scala 88:29 nonToeplitzProcessingElement.scala 97:29 nonToeplitzProcessingElement.scala 79:29 nonToeplitzProcessingElement.scala 88:29 nonToeplitzProcessingElement.scala 105:29 nonToeplitzProcessingElement.scala 112:29 nonToeplitzProcessingElement.scala 88:29 nonToeplitzProcessingElement.scala 79:29 nonToeplitzProcessingElement.scala 88:29 nonToeplitzProcessingElement.scala 105:29]
    weightDataSPadFirstRead <= mux(reset, UInt<1>("h1"), _GEN_169) @[nonToeplitzProcessingElement.scala 80:29 nonToeplitzProcessingElement.scala 80:29 nonToeplitzProcessingElement.scala 89:29 nonToeplitzProcessingElement.scala 89:29 nonToeplitzProcessingElement.scala 80:29 nonToeplitzProcessingElement.scala 89:29 nonToeplitzProcessingElement.scala 89:29 nonToeplitzProcessingElement.scala 80:29 nonToeplitzProcessingElement.scala 89:29 nonToeplitzProcessingElement.scala 472:35]
    productReg <= mux(reset, UInt<21>("h0"), _GEN_175) @[nonToeplitzProcessingElement.scala 442:18]
    pSumSPadLoadReg <= mux(reset, UInt<21>("h0"), pSumSPadLoadWire) @[nonToeplitzProcessingElement.scala 347:19]
    PESPadStateReg <= mux(reset, UInt<3>("h0"), _GEN_164) @[nonToeplitzProcessingElement.scala 75:10 nonToeplitzProcessingElement.scala 75:10 nonToeplitzProcessingElement.scala 84:10 nonToeplitzProcessingElement.scala 116:10 nonToeplitzProcessingElement.scala 84:10 nonToeplitzProcessingElement.scala 93:10 nonToeplitzProcessingElement.scala 75:10 nonToeplitzProcessingElement.scala 84:10 nonToeplitzProcessingElement.scala 101:10 nonToeplitzProcessingElement.scala 434:12 nonToeplitzProcessingElement.scala 437:12 nonToeplitzProcessingElement.scala 441:12 nonToeplitzProcessingElement.scala 116:10 nonToeplitzProcessingElement.scala 84:10 nonToeplitzProcessingElement.scala 75:10 nonToeplitzProcessingElement.scala 84:10 nonToeplitzProcessingElement.scala 101:10]
    currentSliding <= mux(reset, UInt<1>("h0"), _GEN_174) @[nonToeplitzProcessingElement.scala 408:24 nonToeplitzProcessingElement.scala 418:26 nonToeplitzProcessingElement.scala 455:28 nonToeplitzProcessingElement.scala 459:28]
    currentLowerBound <= mux(reset, UInt<4>("h0"), inActDataSPadModule.io_dataPath_slidingBoxLB) @[nonToeplitzProcessingElement.scala 238:21]
    currentUpperBound <= mux(reset, UInt<4>("h0"), inActDataSPadModule.io_dataPath_slidingBoxUB) @[nonToeplitzProcessingElement.scala 239:21]
    pSumLoadStateReg <= mux(reset, UInt<1>("h0"), _T_32) @[nonToeplitzProcessingElement.scala 323:20]
    pSumPadReadIdxReg <= mux(reset, UInt<5>("h0"), _T_44) @[nonToeplitzProcessingElement.scala 354:21]

  module Queue :
    input clock : Clock
    input reset : UInt<1>
    output io_enq_ready : UInt<1>
    input io_enq_valid : UInt<1>
    input io_enq_bits : UInt<7>
    input io_deq_ready : UInt<1>
    output io_deq_valid : UInt<1>
    output io_deq_bits : UInt<7>
    output io_count : UInt<3>
  
    mem ram : @[Decoupled.scala 209:16]
      data-type => UInt<7>
      depth => 4
      read-latency => 0
      write-latency => 1
      reader => _T_15
      writer => _T_5
      read-under-write => undefined
    reg _T : UInt<2>, clock with :
      reset => (UInt<1>("h0"), _T) @[Counter.scala 29:33]
    reg _T_1 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), _T_1) @[Counter.scala 29:33]
    reg maybe_full : UInt<1>, clock with :
      reset => (UInt<1>("h0"), maybe_full) @[Decoupled.scala 212:27]
    node ptr_match = eq(_T, _T_1) @[Decoupled.scala 214:33]
    node _T_2 = eq(maybe_full, UInt<1>("h0")) @[Decoupled.scala 215:28]
    node empty = and(ptr_match, _T_2) @[Decoupled.scala 215:25]
    node full = and(ptr_match, maybe_full) @[Decoupled.scala 216:24]
    node _T_3 = and(io_enq_ready, io_enq_valid) @[Decoupled.scala 40:37]
    node _T_4 = and(io_deq_ready, io_deq_valid) @[Decoupled.scala 40:37]
    node _T_6 = eq(_T, UInt<2>("h3")) @[Counter.scala 38:24]
    node _T_7 = add(_T, UInt<1>("h1")) @[Counter.scala 39:22]
    node _T_8 = tail(_T_7, 1) @[Counter.scala 39:22]
    node _GEN_9 = mux(io_deq_ready, UInt<1>("h0"), _T_3) @[Decoupled.scala 240:27]
    node _GEN_12 = mux(empty, _GEN_9, _T_3) @[Decoupled.scala 237:18]
    node do_enq = _GEN_12 @[Decoupled.scala 240:36]
    node _GEN_0 = validif(do_enq, _T) @[Decoupled.scala 220:17]
    node _GEN_1 = validif(do_enq, clock) @[Decoupled.scala 220:17]
    node _GEN_2 = mux(do_enq, UInt<1>("h1"), UInt<1>("h0")) @[Decoupled.scala 220:17]
    node _GEN_3 = validif(do_enq, UInt<1>("h1")) @[Decoupled.scala 220:17]
    node _GEN_4 = validif(do_enq, io_enq_bits) @[Decoupled.scala 220:17]
    node _GEN_5 = mux(do_enq, _T_8, _T) @[Decoupled.scala 220:17]
    node _T_9 = eq(_T_1, UInt<2>("h3")) @[Counter.scala 38:24]
    node _T_10 = add(_T_1, UInt<1>("h1")) @[Counter.scala 39:22]
    node _T_11 = tail(_T_10, 1) @[Counter.scala 39:22]
    node _GEN_11 = mux(empty, UInt<1>("h0"), _T_4) @[Decoupled.scala 237:18]
    node do_deq = _GEN_11 @[Decoupled.scala 239:14]
    node _GEN_6 = mux(do_deq, _T_11, _T_1) @[Decoupled.scala 224:17]
    node _T_12 = neq(do_enq, do_deq) @[Decoupled.scala 227:16]
    node _GEN_7 = mux(_T_12, do_enq, maybe_full) @[Decoupled.scala 227:28]
    node _T_13 = eq(empty, UInt<1>("h0")) @[Decoupled.scala 231:19]
    node _T_14 = eq(full, UInt<1>("h0")) @[Decoupled.scala 232:19]
    node _GEN_8 = mux(io_enq_valid, UInt<1>("h1"), _T_13) @[Decoupled.scala 236:25]
    node _GEN_10 = mux(empty, io_enq_bits, ram._T_15.data) @[Decoupled.scala 237:18]
    node _GEN_13 = mux(io_deq_ready, UInt<1>("h1"), _T_14) @[Decoupled.scala 245:25]
    node _T_16 = sub(_T, _T_1) @[Decoupled.scala 248:32]
    node ptr_diff = tail(_T_16, 1) @[Decoupled.scala 248:32]
    node _T_17 = and(maybe_full, ptr_match) @[Decoupled.scala 250:32]
    node _T_18 = mux(_T_17, UInt<3>("h4"), UInt<1>("h0")) @[Decoupled.scala 250:20]
    node _T_19 = or(_T_18, ptr_diff) @[Decoupled.scala 250:62]
    io_enq_ready <= _GEN_13 @[Decoupled.scala 232:16 Decoupled.scala 245:40]
    io_deq_valid <= _GEN_8 @[Decoupled.scala 231:16 Decoupled.scala 236:40]
    io_deq_bits <= _GEN_10 @[Decoupled.scala 233:15 Decoupled.scala 238:19]
    io_count <= _T_19 @[Decoupled.scala 250:14]
    ram._T_15.addr <= _T_1 @[Decoupled.scala 233:21]
    ram._T_15.en <= UInt<1>("h1") @[Decoupled.scala 209:16 Decoupled.scala 233:21]
    ram._T_15.clk <= clock @[Decoupled.scala 233:21]
    ram._T_5.addr <= _GEN_0 @[Decoupled.scala 221:8]
    ram._T_5.en <= _GEN_2 @[Decoupled.scala 209:16 Decoupled.scala 221:8]
    ram._T_5.clk <= _GEN_1 @[Decoupled.scala 221:8]
    ram._T_5.data <= _GEN_4 @[Decoupled.scala 221:24]
    ram._T_5.mask <= _GEN_3 @[Decoupled.scala 221:8 Decoupled.scala 221:24]
    _T <= mux(reset, UInt<2>("h0"), _GEN_5) @[Counter.scala 39:13]
    _T_1 <= mux(reset, UInt<2>("h0"), _GEN_6) @[Counter.scala 39:13]
    maybe_full <= mux(reset, UInt<1>("h0"), _GEN_7) @[Decoupled.scala 228:16]

  module Queue_1 :
    input clock : Clock
    input reset : UInt<1>
    output io_enq_ready : UInt<1>
    input io_enq_valid : UInt<1>
    input io_enq_bits : UInt<12>
    input io_deq_ready : UInt<1>
    output io_deq_valid : UInt<1>
    output io_deq_bits : UInt<12>
    output io_count : UInt<3>
  
    mem ram : @[Decoupled.scala 209:16]
      data-type => UInt<12>
      depth => 4
      read-latency => 0
      write-latency => 1
      reader => _T_15
      writer => _T_5
      read-under-write => undefined
    reg _T : UInt<2>, clock with :
      reset => (UInt<1>("h0"), _T) @[Counter.scala 29:33]
    reg _T_1 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), _T_1) @[Counter.scala 29:33]
    reg maybe_full : UInt<1>, clock with :
      reset => (UInt<1>("h0"), maybe_full) @[Decoupled.scala 212:27]
    node ptr_match = eq(_T, _T_1) @[Decoupled.scala 214:33]
    node _T_2 = eq(maybe_full, UInt<1>("h0")) @[Decoupled.scala 215:28]
    node empty = and(ptr_match, _T_2) @[Decoupled.scala 215:25]
    node full = and(ptr_match, maybe_full) @[Decoupled.scala 216:24]
    node _T_3 = and(io_enq_ready, io_enq_valid) @[Decoupled.scala 40:37]
    node _T_4 = and(io_deq_ready, io_deq_valid) @[Decoupled.scala 40:37]
    node _T_6 = eq(_T, UInt<2>("h3")) @[Counter.scala 38:24]
    node _T_7 = add(_T, UInt<1>("h1")) @[Counter.scala 39:22]
    node _T_8 = tail(_T_7, 1) @[Counter.scala 39:22]
    node _GEN_9 = mux(io_deq_ready, UInt<1>("h0"), _T_3) @[Decoupled.scala 240:27]
    node _GEN_12 = mux(empty, _GEN_9, _T_3) @[Decoupled.scala 237:18]
    node do_enq = _GEN_12 @[Decoupled.scala 240:36]
    node _GEN_0 = validif(do_enq, _T) @[Decoupled.scala 220:17]
    node _GEN_1 = validif(do_enq, clock) @[Decoupled.scala 220:17]
    node _GEN_2 = mux(do_enq, UInt<1>("h1"), UInt<1>("h0")) @[Decoupled.scala 220:17]
    node _GEN_3 = validif(do_enq, UInt<1>("h1")) @[Decoupled.scala 220:17]
    node _GEN_4 = validif(do_enq, io_enq_bits) @[Decoupled.scala 220:17]
    node _GEN_5 = mux(do_enq, _T_8, _T) @[Decoupled.scala 220:17]
    node _T_9 = eq(_T_1, UInt<2>("h3")) @[Counter.scala 38:24]
    node _T_10 = add(_T_1, UInt<1>("h1")) @[Counter.scala 39:22]
    node _T_11 = tail(_T_10, 1) @[Counter.scala 39:22]
    node _GEN_11 = mux(empty, UInt<1>("h0"), _T_4) @[Decoupled.scala 237:18]
    node do_deq = _GEN_11 @[Decoupled.scala 239:14]
    node _GEN_6 = mux(do_deq, _T_11, _T_1) @[Decoupled.scala 224:17]
    node _T_12 = neq(do_enq, do_deq) @[Decoupled.scala 227:16]
    node _GEN_7 = mux(_T_12, do_enq, maybe_full) @[Decoupled.scala 227:28]
    node _T_13 = eq(empty, UInt<1>("h0")) @[Decoupled.scala 231:19]
    node _T_14 = eq(full, UInt<1>("h0")) @[Decoupled.scala 232:19]
    node _GEN_8 = mux(io_enq_valid, UInt<1>("h1"), _T_13) @[Decoupled.scala 236:25]
    node _GEN_10 = mux(empty, io_enq_bits, ram._T_15.data) @[Decoupled.scala 237:18]
    node _GEN_13 = mux(io_deq_ready, UInt<1>("h1"), _T_14) @[Decoupled.scala 245:25]
    node _T_16 = sub(_T, _T_1) @[Decoupled.scala 248:32]
    node ptr_diff = tail(_T_16, 1) @[Decoupled.scala 248:32]
    node _T_17 = and(maybe_full, ptr_match) @[Decoupled.scala 250:32]
    node _T_18 = mux(_T_17, UInt<3>("h4"), UInt<1>("h0")) @[Decoupled.scala 250:20]
    node _T_19 = or(_T_18, ptr_diff) @[Decoupled.scala 250:62]
    io_enq_ready <= _GEN_13 @[Decoupled.scala 232:16 Decoupled.scala 245:40]
    io_deq_valid <= _GEN_8 @[Decoupled.scala 231:16 Decoupled.scala 236:40]
    io_deq_bits <= _GEN_10 @[Decoupled.scala 233:15 Decoupled.scala 238:19]
    io_count <= _T_19 @[Decoupled.scala 250:14]
    ram._T_15.addr <= _T_1 @[Decoupled.scala 233:21]
    ram._T_15.en <= UInt<1>("h1") @[Decoupled.scala 209:16 Decoupled.scala 233:21]
    ram._T_15.clk <= clock @[Decoupled.scala 233:21]
    ram._T_5.addr <= _GEN_0 @[Decoupled.scala 221:8]
    ram._T_5.en <= _GEN_2 @[Decoupled.scala 209:16 Decoupled.scala 221:8]
    ram._T_5.clk <= _GEN_1 @[Decoupled.scala 221:8]
    ram._T_5.data <= _GEN_4 @[Decoupled.scala 221:24]
    ram._T_5.mask <= _GEN_3 @[Decoupled.scala 221:8 Decoupled.scala 221:24]
    _T <= mux(reset, UInt<2>("h0"), _GEN_5) @[Counter.scala 39:13]
    _T_1 <= mux(reset, UInt<2>("h0"), _GEN_6) @[Counter.scala 39:13]
    maybe_full <= mux(reset, UInt<1>("h0"), _GEN_7) @[Decoupled.scala 228:16]

  module Queue_4 :
    input clock : Clock
    input reset : UInt<1>
    output io_enq_ready : UInt<1>
    input io_enq_valid : UInt<1>
    input io_enq_bits : UInt<21>
    input io_deq_ready : UInt<1>
    output io_deq_valid : UInt<1>
    output io_deq_bits : UInt<21>
    output io_count : UInt<3>
  
    mem ram : @[Decoupled.scala 209:16]
      data-type => UInt<21>
      depth => 4
      read-latency => 0
      write-latency => 1
      reader => _T_15
      writer => _T_5
      read-under-write => undefined
    reg _T : UInt<2>, clock with :
      reset => (UInt<1>("h0"), _T) @[Counter.scala 29:33]
    reg _T_1 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), _T_1) @[Counter.scala 29:33]
    reg maybe_full : UInt<1>, clock with :
      reset => (UInt<1>("h0"), maybe_full) @[Decoupled.scala 212:27]
    node ptr_match = eq(_T, _T_1) @[Decoupled.scala 214:33]
    node _T_2 = eq(maybe_full, UInt<1>("h0")) @[Decoupled.scala 215:28]
    node empty = and(ptr_match, _T_2) @[Decoupled.scala 215:25]
    node full = and(ptr_match, maybe_full) @[Decoupled.scala 216:24]
    node _T_3 = and(io_enq_ready, io_enq_valid) @[Decoupled.scala 40:37]
    node _T_4 = and(io_deq_ready, io_deq_valid) @[Decoupled.scala 40:37]
    node _T_6 = eq(_T, UInt<2>("h3")) @[Counter.scala 38:24]
    node _T_7 = add(_T, UInt<1>("h1")) @[Counter.scala 39:22]
    node _T_8 = tail(_T_7, 1) @[Counter.scala 39:22]
    node _GEN_9 = mux(io_deq_ready, UInt<1>("h0"), _T_3) @[Decoupled.scala 240:27]
    node _GEN_12 = mux(empty, _GEN_9, _T_3) @[Decoupled.scala 237:18]
    node do_enq = _GEN_12 @[Decoupled.scala 240:36]
    node _GEN_0 = validif(do_enq, _T) @[Decoupled.scala 220:17]
    node _GEN_1 = validif(do_enq, clock) @[Decoupled.scala 220:17]
    node _GEN_2 = mux(do_enq, UInt<1>("h1"), UInt<1>("h0")) @[Decoupled.scala 220:17]
    node _GEN_3 = validif(do_enq, UInt<1>("h1")) @[Decoupled.scala 220:17]
    node _GEN_4 = validif(do_enq, io_enq_bits) @[Decoupled.scala 220:17]
    node _GEN_5 = mux(do_enq, _T_8, _T) @[Decoupled.scala 220:17]
    node _T_9 = eq(_T_1, UInt<2>("h3")) @[Counter.scala 38:24]
    node _T_10 = add(_T_1, UInt<1>("h1")) @[Counter.scala 39:22]
    node _T_11 = tail(_T_10, 1) @[Counter.scala 39:22]
    node _GEN_11 = mux(empty, UInt<1>("h0"), _T_4) @[Decoupled.scala 237:18]
    node do_deq = _GEN_11 @[Decoupled.scala 239:14]
    node _GEN_6 = mux(do_deq, _T_11, _T_1) @[Decoupled.scala 224:17]
    node _T_12 = neq(do_enq, do_deq) @[Decoupled.scala 227:16]
    node _GEN_7 = mux(_T_12, do_enq, maybe_full) @[Decoupled.scala 227:28]
    node _T_13 = eq(empty, UInt<1>("h0")) @[Decoupled.scala 231:19]
    node _T_14 = eq(full, UInt<1>("h0")) @[Decoupled.scala 232:19]
    node _GEN_8 = mux(io_enq_valid, UInt<1>("h1"), _T_13) @[Decoupled.scala 236:25]
    node _GEN_10 = mux(empty, io_enq_bits, ram._T_15.data) @[Decoupled.scala 237:18]
    node _GEN_13 = mux(io_deq_ready, UInt<1>("h1"), _T_14) @[Decoupled.scala 245:25]
    node _T_16 = sub(_T, _T_1) @[Decoupled.scala 248:32]
    node ptr_diff = tail(_T_16, 1) @[Decoupled.scala 248:32]
    node _T_17 = and(maybe_full, ptr_match) @[Decoupled.scala 250:32]
    node _T_18 = mux(_T_17, UInt<3>("h4"), UInt<1>("h0")) @[Decoupled.scala 250:20]
    node _T_19 = or(_T_18, ptr_diff) @[Decoupled.scala 250:62]
    io_enq_ready <= _GEN_13 @[Decoupled.scala 232:16 Decoupled.scala 245:40]
    io_deq_valid <= _GEN_8 @[Decoupled.scala 231:16 Decoupled.scala 236:40]
    io_deq_bits <= _GEN_10 @[Decoupled.scala 233:15 Decoupled.scala 238:19]
    io_count <= _T_19 @[Decoupled.scala 250:14]
    ram._T_15.addr <= _T_1 @[Decoupled.scala 233:21]
    ram._T_15.en <= UInt<1>("h1") @[Decoupled.scala 209:16 Decoupled.scala 233:21]
    ram._T_15.clk <= clock @[Decoupled.scala 233:21]
    ram._T_5.addr <= _GEN_0 @[Decoupled.scala 221:8]
    ram._T_5.en <= _GEN_2 @[Decoupled.scala 209:16 Decoupled.scala 221:8]
    ram._T_5.clk <= _GEN_1 @[Decoupled.scala 221:8]
    ram._T_5.data <= _GEN_4 @[Decoupled.scala 221:24]
    ram._T_5.mask <= _GEN_3 @[Decoupled.scala 221:8 Decoupled.scala 221:24]
    _T <= mux(reset, UInt<2>("h0"), _GEN_5) @[Counter.scala 39:13]
    _T_1 <= mux(reset, UInt<2>("h0"), _GEN_6) @[Counter.scala 39:13]
    maybe_full <= mux(reset, UInt<1>("h0"), _GEN_7) @[Decoupled.scala 228:16]

  module nonToeplitzProcessingElement :
    input clock : Clock
    input reset : UInt<1>
    output io_dataStream_ipsIO_ready : UInt<1>
    input io_dataStream_ipsIO_valid : UInt<1>
    input io_dataStream_ipsIO_bits : UInt<21>
    input io_dataStream_opsIO_ready : UInt<1>
    output io_dataStream_opsIO_valid : UInt<1>
    output io_dataStream_opsIO_bits : UInt<21>
    output io_dataStream_inActIOs_adrIOs_data_ready : UInt<1>
    input io_dataStream_inActIOs_adrIOs_data_valid : UInt<1>
    input io_dataStream_inActIOs_adrIOs_data_bits : UInt<7>
    output io_dataStream_inActIOs_dataIOs_data_ready : UInt<1>
    input io_dataStream_inActIOs_dataIOs_data_valid : UInt<1>
    input io_dataStream_inActIOs_dataIOs_data_bits : UInt<12>
    output io_dataStream_weightIOs_adrIOs_data_ready : UInt<1>
    input io_dataStream_weightIOs_adrIOs_data_valid : UInt<1>
    input io_dataStream_weightIOs_adrIOs_data_bits : UInt<7>
    output io_dataStream_weightIOs_dataIOs_data_ready : UInt<1>
    input io_dataStream_weightIOs_dataIOs_data_valid : UInt<1>
    input io_dataStream_weightIOs_dataIOs_data_bits : UInt<12>
    output io_padWF_inActWriteFin_adrWriteFin : UInt<1>
    output io_padWF_inActWriteFin_dataWriteFin : UInt<1>
    output io_padWF_weightWriteFin_adrWriteFin : UInt<1>
    output io_padWF_weightWriteFin_dataWriteFin : UInt<1>
    output io_padWF_pSumAddFin : UInt<1>
    input io_topCtrl_pSumEnqEn : UInt<1>
    input io_topCtrl_doLoadEn : UInt<1>
    output io_topCtrl_writeFinish : UInt<1>
    output io_topCtrl_calFinish : UInt<1>
    output io_debugIO_peControlDebugIO_peState : UInt<2>
    output io_debugIO_peControlDebugIO_doMACEnDebug : UInt<1>
    output io_debugIO_peSPadDebugIO_inActMatrixData : UInt<8>
    output io_debugIO_peSPadDebugIO_inActMatrixRow : UInt<4>
    output io_debugIO_peSPadDebugIO_inActMatrixColumn : UInt<7>
    output io_debugIO_peSPadDebugIO_inActAdrInc : UInt<1>
    output io_debugIO_peSPadDebugIO_inActDataInc : UInt<1>
    output io_debugIO_peSPadDebugIO_inActAdrIdx : UInt<7>
    output io_debugIO_peSPadDebugIO_weightAdrSPadReadOut : UInt<7>
    output io_debugIO_peSPadDebugIO_weightMatrixData : UInt<8>
    output io_debugIO_peSPadDebugIO_weightMatrixRow : UInt<4>
    output io_debugIO_peSPadDebugIO_productResult : UInt<21>
    output io_debugIO_peSPadDebugIO_pSumResult : UInt<21>
    output io_debugIO_peSPadDebugIO_pSumLoad : UInt<21>
    output io_debugIO_peSPadDebugIO_weightAdrInIdx : UInt<4>
    output io_debugIO_peSPadDebugIO_sPadState : UInt<3>
    output io_debugIO_peSPadDebugIO_pSumReadIdx : UInt<5>
    output io_debugIO_peSPadDebugIO_pSumPadReadIdx : UInt<5>
    output io_debugIO_peSPadDebugIO_inActDataSliding : UInt<1>
    output io_debugIO_peSPadDebugIO_inActDataSlidingFire : UInt<1>
    output io_debugIO_peSPadDebugIO_futureLBStart : UInt<4>
    output io_debugIO_peSPadDebugIO_inActDataIndex : UInt<6>
    output io_debugIO_peSPadDebugIO_inActAdrData : UInt<7>
    output io_debugIO_writeFinishRegVec_0 : UInt<1>
    output io_debugIO_writeFinishRegVec_1 : UInt<1>
    output io_debugIO_writeFinishRegVec_2 : UInt<1>
    output io_debugIO_writeFinishRegVec_3 : UInt<1>
  
    inst peCtrlModule of ProcessingElementControl @[nonToeplitzProcessingElement.scala 10:58]
    inst nonToeplitzpePadModule of nonToeplitzProcessingElementPad @[nonToeplitzProcessingElement.scala 13:64]
    inst Queue of Queue @[Decoupled.scala 287:21]
    inst Queue_1 of Queue_1 @[Decoupled.scala 287:21]
    inst Queue_2 of Queue @[Decoupled.scala 287:21]
    inst Queue_3 of Queue_1 @[Decoupled.scala 287:21]
    inst Queue_4 of Queue_4 @[Decoupled.scala 287:21]
    inst Queue_5 of Queue_4 @[Decoupled.scala 287:21]
    reg inActAdrWFReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), inActAdrWFReg) @[nonToeplitzProcessingElement.scala 39:82]
    reg inActDataWFReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), inActDataWFReg) @[nonToeplitzProcessingElement.scala 39:82]
    reg weightAdrWFReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), weightAdrWFReg) @[nonToeplitzProcessingElement.scala 39:82]
    reg weightDataWFReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), weightDataWFReg) @[nonToeplitzProcessingElement.scala 39:82]
    node _T = mux(nonToeplitzpePadModule.io_padWF_inActWriteFin_adrWriteFin, UInt<1>("h1"), inActAdrWFReg) @[nonToeplitzProcessingElement.scala 45:62]
    node _T_8 = and(inActAdrWFReg, inActDataWFReg) @[nonToeplitzProcessingElement.scala 47:49]
    node _T_9 = and(_T_8, weightAdrWFReg) @[nonToeplitzProcessingElement.scala 47:49]
    node _T_10 = and(_T_9, weightDataWFReg) @[nonToeplitzProcessingElement.scala 47:49]
    node inActAndWeightWFWire = _T_10 @[nonToeplitzProcessingElement.scala 37:45 nonToeplitzProcessingElement.scala 47:19]
    node _T_1 = mux(inActAndWeightWFWire, UInt<1>("h0"), _T) @[nonToeplitzProcessingElement.scala 45:32]
    node _T_2 = mux(nonToeplitzpePadModule.io_padWF_inActWriteFin_dataWriteFin, UInt<1>("h1"), inActDataWFReg) @[nonToeplitzProcessingElement.scala 45:62]
    node _T_3 = mux(inActAndWeightWFWire, UInt<1>("h0"), _T_2) @[nonToeplitzProcessingElement.scala 45:32]
    node _T_4 = mux(nonToeplitzpePadModule.io_padWF_weightWriteFin_adrWriteFin, UInt<1>("h1"), weightAdrWFReg) @[nonToeplitzProcessingElement.scala 45:62]
    node _T_5 = mux(inActAndWeightWFWire, UInt<1>("h0"), _T_4) @[nonToeplitzProcessingElement.scala 45:32]
    node _T_6 = mux(nonToeplitzpePadModule.io_padWF_weightWriteFin_dataWriteFin, UInt<1>("h1"), weightDataWFReg) @[nonToeplitzProcessingElement.scala 45:62]
    node _T_7 = mux(inActAndWeightWFWire, UInt<1>("h0"), _T_6) @[nonToeplitzProcessingElement.scala 45:32]
    io_dataStream_ipsIO_ready <= Queue_4.io_enq_ready @[Decoupled.scala 290:17]
    io_dataStream_opsIO_valid <= Queue_5.io_deq_valid @[nonToeplitzProcessingElement.scala 52:23]
    io_dataStream_opsIO_bits <= Queue_5.io_deq_bits @[nonToeplitzProcessingElement.scala 52:23]
    io_dataStream_inActIOs_adrIOs_data_ready <= Queue.io_enq_ready @[Decoupled.scala 290:17]
    io_dataStream_inActIOs_dataIOs_data_ready <= Queue_1.io_enq_ready @[Decoupled.scala 290:17]
    io_dataStream_weightIOs_adrIOs_data_ready <= Queue_2.io_enq_ready @[Decoupled.scala 290:17]
    io_dataStream_weightIOs_dataIOs_data_ready <= Queue_3.io_enq_ready @[Decoupled.scala 290:17]
    io_padWF_inActWriteFin_adrWriteFin <= nonToeplitzpePadModule.io_padWF_inActWriteFin_adrWriteFin @[nonToeplitzProcessingElement.scala 29:76]
    io_padWF_inActWriteFin_dataWriteFin <= nonToeplitzpePadModule.io_padWF_inActWriteFin_dataWriteFin @[nonToeplitzProcessingElement.scala 29:76]
    io_padWF_weightWriteFin_adrWriteFin <= nonToeplitzpePadModule.io_padWF_weightWriteFin_adrWriteFin @[nonToeplitzProcessingElement.scala 29:76]
    io_padWF_weightWriteFin_dataWriteFin <= nonToeplitzpePadModule.io_padWF_weightWriteFin_dataWriteFin @[nonToeplitzProcessingElement.scala 29:76]
    io_padWF_pSumAddFin <= nonToeplitzpePadModule.io_padWF_pSumAddFin @[nonToeplitzProcessingElement.scala 30:23]
    io_topCtrl_writeFinish <= inActAndWeightWFWire @[nonToeplitzProcessingElement.scala 48:26]
    io_topCtrl_calFinish <= peCtrlModule.io_ctrlTop_calFinish @[nonToeplitzProcessingElement.scala 33:24]
    io_debugIO_peControlDebugIO_peState <= peCtrlModule.io_debugIO_peState @[nonToeplitzProcessingElement.scala 54:33]
    io_debugIO_peControlDebugIO_doMACEnDebug <= peCtrlModule.io_debugIO_doMACEnDebug @[nonToeplitzProcessingElement.scala 54:33]
    io_debugIO_peSPadDebugIO_inActMatrixData <= nonToeplitzpePadModule.io_debugIO_inActMatrixData @[nonToeplitzProcessingElement.scala 55:30]
    io_debugIO_peSPadDebugIO_inActMatrixRow <= nonToeplitzpePadModule.io_debugIO_inActMatrixRow @[nonToeplitzProcessingElement.scala 55:30]
    io_debugIO_peSPadDebugIO_inActMatrixColumn <= nonToeplitzpePadModule.io_debugIO_inActMatrixColumn @[nonToeplitzProcessingElement.scala 55:30]
    io_debugIO_peSPadDebugIO_inActAdrInc <= nonToeplitzpePadModule.io_debugIO_inActAdrInc @[nonToeplitzProcessingElement.scala 55:30]
    io_debugIO_peSPadDebugIO_inActDataInc <= nonToeplitzpePadModule.io_debugIO_inActDataInc @[nonToeplitzProcessingElement.scala 55:30]
    io_debugIO_peSPadDebugIO_inActAdrIdx <= nonToeplitzpePadModule.io_debugIO_inActAdrIdx @[nonToeplitzProcessingElement.scala 55:30]
    io_debugIO_peSPadDebugIO_weightAdrSPadReadOut <= nonToeplitzpePadModule.io_debugIO_weightAdrSPadReadOut @[nonToeplitzProcessingElement.scala 55:30]
    io_debugIO_peSPadDebugIO_weightMatrixData <= nonToeplitzpePadModule.io_debugIO_weightMatrixData @[nonToeplitzProcessingElement.scala 55:30]
    io_debugIO_peSPadDebugIO_weightMatrixRow <= nonToeplitzpePadModule.io_debugIO_weightMatrixRow @[nonToeplitzProcessingElement.scala 55:30]
    io_debugIO_peSPadDebugIO_productResult <= nonToeplitzpePadModule.io_debugIO_productResult @[nonToeplitzProcessingElement.scala 55:30]
    io_debugIO_peSPadDebugIO_pSumResult <= nonToeplitzpePadModule.io_debugIO_pSumResult @[nonToeplitzProcessingElement.scala 55:30]
    io_debugIO_peSPadDebugIO_pSumLoad <= nonToeplitzpePadModule.io_debugIO_pSumLoad @[nonToeplitzProcessingElement.scala 55:30]
    io_debugIO_peSPadDebugIO_weightAdrInIdx <= nonToeplitzpePadModule.io_debugIO_weightAdrInIdx @[nonToeplitzProcessingElement.scala 55:30]
    io_debugIO_peSPadDebugIO_sPadState <= nonToeplitzpePadModule.io_debugIO_sPadState @[nonToeplitzProcessingElement.scala 55:30]
    io_debugIO_peSPadDebugIO_pSumReadIdx <= nonToeplitzpePadModule.io_debugIO_pSumReadIdx @[nonToeplitzProcessingElement.scala 55:30]
    io_debugIO_peSPadDebugIO_pSumPadReadIdx <= nonToeplitzpePadModule.io_debugIO_pSumPadReadIdx @[nonToeplitzProcessingElement.scala 55:30]
    io_debugIO_peSPadDebugIO_inActDataSliding <= nonToeplitzpePadModule.io_debugIO_inActDataSliding @[nonToeplitzProcessingElement.scala 55:30]
    io_debugIO_peSPadDebugIO_inActDataSlidingFire <= nonToeplitzpePadModule.io_debugIO_inActDataSlidingFire @[nonToeplitzProcessingElement.scala 55:30]
    io_debugIO_peSPadDebugIO_futureLBStart <= nonToeplitzpePadModule.io_debugIO_futureLBStart @[nonToeplitzProcessingElement.scala 55:30]
    io_debugIO_peSPadDebugIO_inActDataIndex <= nonToeplitzpePadModule.io_debugIO_inActDataIndex @[nonToeplitzProcessingElement.scala 55:30]
    io_debugIO_peSPadDebugIO_inActAdrData <= nonToeplitzpePadModule.io_debugIO_inActAdrData @[nonToeplitzProcessingElement.scala 55:30]
    io_debugIO_writeFinishRegVec_0 <= inActAdrWFReg @[nonToeplitzProcessingElement.scala 56:34]
    io_debugIO_writeFinishRegVec_1 <= inActDataWFReg @[nonToeplitzProcessingElement.scala 56:34]
    io_debugIO_writeFinishRegVec_2 <= weightAdrWFReg @[nonToeplitzProcessingElement.scala 56:34]
    io_debugIO_writeFinishRegVec_3 <= weightDataWFReg @[nonToeplitzProcessingElement.scala 56:34]
    peCtrlModule.clock <= clock
    peCtrlModule.reset <= reset
    peCtrlModule.io_ctrlPad_fromTopIO_writeFinish <= nonToeplitzpePadModule.io_padCtrl_fromTopIO_writeFinish @[nonToeplitzProcessingElement.scala 31:20]
    peCtrlModule.io_ctrlPad_fromTopIO_calFinish <= nonToeplitzpePadModule.io_padCtrl_fromTopIO_calFinish @[nonToeplitzProcessingElement.scala 31:20]
    peCtrlModule.io_ctrlTop_pSumEnqEn <= io_topCtrl_pSumEnqEn @[nonToeplitzProcessingElement.scala 32:30]
    peCtrlModule.io_ctrlTop_doLoadEn <= io_topCtrl_doLoadEn @[nonToeplitzProcessingElement.scala 34:29]
    peCtrlModule.io_ctrlTop_writeFinish <= inActAndWeightWFWire @[nonToeplitzProcessingElement.scala 49:32]
    nonToeplitzpePadModule.clock <= clock
    nonToeplitzpePadModule.reset <= reset
    nonToeplitzpePadModule.io_padCtrl_doMACEn <= peCtrlModule.io_ctrlPad_doMACEn @[nonToeplitzProcessingElement.scala 31:20]
    nonToeplitzpePadModule.io_padCtrl_fromTopIO_pSumEnqEn <= peCtrlModule.io_ctrlPad_fromTopIO_pSumEnqEn @[nonToeplitzProcessingElement.scala 31:20]
    nonToeplitzpePadModule.io_padCtrl_fromTopIO_doLoadEn <= peCtrlModule.io_ctrlPad_fromTopIO_doLoadEn @[nonToeplitzProcessingElement.scala 31:20]
    nonToeplitzpePadModule.io_dataStream_ipsIO_valid <= Queue_4.io_deq_valid @[nonToeplitzProcessingElement.scala 51:28]
    nonToeplitzpePadModule.io_dataStream_ipsIO_bits <= Queue_4.io_deq_bits @[nonToeplitzProcessingElement.scala 51:28]
    nonToeplitzpePadModule.io_dataStream_opsIO_ready <= Queue_5.io_enq_ready @[Decoupled.scala 290:17]
    nonToeplitzpePadModule.io_dataStream_inActIOs_adrIOs_data_valid <= Queue.io_deq_valid @[nonToeplitzProcessingElement.scala 19:45]
    nonToeplitzpePadModule.io_dataStream_inActIOs_adrIOs_data_bits <= Queue.io_deq_bits @[nonToeplitzProcessingElement.scala 19:45]
    nonToeplitzpePadModule.io_dataStream_inActIOs_dataIOs_data_valid <= Queue_1.io_deq_valid @[nonToeplitzProcessingElement.scala 20:46]
    nonToeplitzpePadModule.io_dataStream_inActIOs_dataIOs_data_bits <= Queue_1.io_deq_bits @[nonToeplitzProcessingElement.scala 20:46]
    nonToeplitzpePadModule.io_dataStream_weightIOs_adrIOs_data_valid <= Queue_2.io_deq_valid @[nonToeplitzProcessingElement.scala 21:46]
    nonToeplitzpePadModule.io_dataStream_weightIOs_adrIOs_data_bits <= Queue_2.io_deq_bits @[nonToeplitzProcessingElement.scala 21:46]
    nonToeplitzpePadModule.io_dataStream_weightIOs_dataIOs_data_valid <= Queue_3.io_deq_valid @[nonToeplitzProcessingElement.scala 22:47]
    nonToeplitzpePadModule.io_dataStream_weightIOs_dataIOs_data_bits <= Queue_3.io_deq_bits @[nonToeplitzProcessingElement.scala 22:47]
    Queue.clock <= clock
    Queue.reset <= reset
    Queue.io_enq_valid <= io_dataStream_inActIOs_adrIOs_data_valid @[Decoupled.scala 288:22]
    Queue.io_enq_bits <= io_dataStream_inActIOs_adrIOs_data_bits @[Decoupled.scala 289:21]
    Queue.io_deq_ready <= nonToeplitzpePadModule.io_dataStream_inActIOs_adrIOs_data_ready @[nonToeplitzProcessingElement.scala 19:45]
    Queue_1.clock <= clock
    Queue_1.reset <= reset
    Queue_1.io_enq_valid <= io_dataStream_inActIOs_dataIOs_data_valid @[Decoupled.scala 288:22]
    Queue_1.io_enq_bits <= io_dataStream_inActIOs_dataIOs_data_bits @[Decoupled.scala 289:21]
    Queue_1.io_deq_ready <= nonToeplitzpePadModule.io_dataStream_inActIOs_dataIOs_data_ready @[nonToeplitzProcessingElement.scala 20:46]
    Queue_2.clock <= clock
    Queue_2.reset <= reset
    Queue_2.io_enq_valid <= io_dataStream_weightIOs_adrIOs_data_valid @[Decoupled.scala 288:22]
    Queue_2.io_enq_bits <= io_dataStream_weightIOs_adrIOs_data_bits @[Decoupled.scala 289:21]
    Queue_2.io_deq_ready <= nonToeplitzpePadModule.io_dataStream_weightIOs_adrIOs_data_ready @[nonToeplitzProcessingElement.scala 21:46]
    Queue_3.clock <= clock
    Queue_3.reset <= reset
    Queue_3.io_enq_valid <= io_dataStream_weightIOs_dataIOs_data_valid @[Decoupled.scala 288:22]
    Queue_3.io_enq_bits <= io_dataStream_weightIOs_dataIOs_data_bits @[Decoupled.scala 289:21]
    Queue_3.io_deq_ready <= nonToeplitzpePadModule.io_dataStream_weightIOs_dataIOs_data_ready @[nonToeplitzProcessingElement.scala 22:47]
    inActAdrWFReg <= mux(reset, UInt<1>("h0"), _T_1) @[nonToeplitzProcessingElement.scala 45:26]
    inActDataWFReg <= mux(reset, UInt<1>("h0"), _T_3) @[nonToeplitzProcessingElement.scala 45:26]
    weightAdrWFReg <= mux(reset, UInt<1>("h0"), _T_5) @[nonToeplitzProcessingElement.scala 45:26]
    weightDataWFReg <= mux(reset, UInt<1>("h0"), _T_7) @[nonToeplitzProcessingElement.scala 45:26]
    Queue_4.clock <= clock
    Queue_4.reset <= reset
    Queue_4.io_enq_valid <= io_dataStream_ipsIO_valid @[Decoupled.scala 288:22]
    Queue_4.io_enq_bits <= io_dataStream_ipsIO_bits @[Decoupled.scala 289:21]
    Queue_4.io_deq_ready <= nonToeplitzpePadModule.io_dataStream_ipsIO_ready @[nonToeplitzProcessingElement.scala 51:28]
    Queue_5.clock <= clock
    Queue_5.reset <= reset
    Queue_5.io_enq_valid <= nonToeplitzpePadModule.io_dataStream_opsIO_valid @[Decoupled.scala 288:22]
    Queue_5.io_enq_bits <= nonToeplitzpePadModule.io_dataStream_opsIO_bits @[Decoupled.scala 289:21]
    Queue_5.io_deq_ready <= io_dataStream_opsIO_ready @[nonToeplitzProcessingElement.scala 52:23]
