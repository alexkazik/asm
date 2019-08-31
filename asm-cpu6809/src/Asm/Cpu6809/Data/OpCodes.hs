{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Asm.Cpu6809.Data.OpCodes
  ( AddressMode(..)
  , module Asm.Cpu6809.Data.OpCodes
  ) where

import           Asm.Core.Prelude
import           Text.Heredoc                  (here)

import           Asm.Cpu6809.OpCodes.Generator

data Register
  = RegA
  | RegB
  | RegD
  | RegX
  | RegY
  | RegS
  | RegU
  | RegPC
  | RegCC
  | RegDP
  deriving (Typeable,Data,Show,Eq,Ord)

data IndexedMode
  = IMIndirect
  | IMOffset Register -- register (+expr) ; expr is int with offset (incl PC)
  | IMRelOffset Register -- register (+expr) ; expr is addr, offset will be calculated (incl PC)
  | IMRegOffset Register Register -- register (a,b,d)
  | IMIncrement Register Bool Bool -- register backward double
  deriving (Typeable,Data,Show,Eq,Ord)

{-
  The following will be defined:

  newtype CpuVariant = CpuVariant {fromCpuVariant :: Int}
    (needs to be defined in the splice since it can't access data created in this file
     and defining it outside would create a orphan instance)

  map with all all cpus:
    cpuVariants :: Map Text CpuVariant

  functions for all cpus:
    cpuVariant6809 :: CpuVariant
    cpuVariant6309 :: CpuVariant

  newtype Operator = Operator {fromOperator :: Int}
    (needs to be defined in the splice since it can't access data created in this file
     and defining it outside would create a orphan instance)

  functions for all operators:
    oprRTS :: Operator
    ...

  instance Pretty Operator where
    with oprRTS prints "rts"
    ...

  opcodeNames :: [(Text, Operator)]
    a list of all operators

  opcodes :: IntMap (IntMap (Map AddressMode (Word16, Bool, Maybe FunctionKey)))
    the first IntMap is an unpacked Map CpuVariant
    the second IntMap is an unpacked Map Operator
-}

mkOpCodes
  -- opcodes
  [here|

  # page 1

  neg.dir      00 [6809,6309] byte
  # oim.immdir 01      [6309] byte
  # aim.immdir 02      [6309] byte
  com.dir      03 [6809,6309] byte
  lsr.dir      04 [6809,6309] byte
  # eim.immdir 05      [6309] byte
  ror.dir      06 [6809,6309] byte
  asr.dir      07 [6809,6309] byte
  lsl.dir      08 [6809,6309] byte
  rol.dir      09 [6809,6309] byte
  dec.dir      0a [6809,6309] byte
  # tim.immdir 0b      [6309] byte
  inc.dir      0c [6809,6309] byte
  tst.dir      0d [6809,6309] byte
  jmp.dir      0e [6809,6309] code
  clr.dir      0f [6809,6309] byte

  nop.imp      12 [6809,6309]
  sync.imp     13 [6809,6309]
  sexw.imp     14      [6309]
  lbra.rel16   16 [6809,6309]
  lbsr.rel16   17 [6809,6309]
  daa.imp      19 [6809,6309]
  orcc.imm8    1a [6809,6309]
  andcc.imm8   1c [6809,6309]
  sex.imp      1d [6809,6309]
  exg.imm8     1e [6809,6309]
  tfr.imm8     1f [6809,6309]

  bra.rel8     20 [6809,6309]
  brn.rel8     21 [6809,6309]
  bhi.rel8     22 [6809,6309]
  bls.rel8     23 [6809,6309]
  bcc.rel8     24 [6809,6309]
  bcs.rel8     25 [6809,6309]
  bne.rel8     26 [6809,6309]
  beq.rel8     27 [6809,6309]
  bvc.rel8     28 [6809,6309]
  bvs.rel8     29 [6809,6309]
  bpl.rel8     2a [6809,6309]
  bmi.rel8     2b [6809,6309]
  bge.rel8     2c [6809,6309]
  blt.rel8     2d [6809,6309]
  bgt.rel8     2e [6809,6309]
  ble.rel8     2f [6809,6309]

  leax.idx     30 [6809,6309] byte
  leay.idx     31 [6809,6309] byte
  leas.idx     32 [6809,6309] byte
  leau.idx     33 [6809,6309] byte
  pshs.imm8    34 [6809,6309]
  puls.imm8    35 [6809,6309]
  pshu.imm8    36 [6809,6309]
  pulu.imm8    37 [6809,6309]
  rts.imp      39 [6809,6309]
  abx.imp      3a [6809,6309]
  rti.imp      3b [6809,6309]
  cwai.imp     3c [6809,6309]
  mul.imp      3d [6809,6309]
  reset.imp    3e [6809,6309] # the only undocumented opcode
  swi.imp      3f [6809,6309]

  nega.imp     40 [6809,6309]
  coma.imp     43 [6809,6309]
  lsra.imp     44 [6809,6309]
  rora.imp     46 [6809,6309]
  asra.imp     47 [6809,6309]
  lsla.imp     48 [6809,6309]
  rola.imp     49 [6809,6309]
  deca.imp     4a [6809,6309]
  inca.imp     4c [6809,6309]
  tsta.imp     4d [6809,6309]
  clra.imp     4f [6809,6309]

  negb.imp     50 [6809,6309]
  comb.imp     53 [6809,6309]
  lsrb.imp     54 [6809,6309]
  rorb.imp     56 [6809,6309]
  asrb.imp     57 [6809,6309]
  lslb.imp     58 [6809,6309]
  rolb.imp     59 [6809,6309]
  decb.imp     5a [6809,6309]
  incb.imp     5c [6809,6309]
  tstb.imp     5d [6809,6309]
  clrb.imp     5f [6809,6309]

  neg.idx      60 [6809,6309] byte
  # oim.immidx 61      [6309] byte
  # aim.immidx 62      [6309] byte
  com.idx      63 [6809,6309] byte
  lsr.idx      64 [6809,6309] byte
  # eim.immidx 65      [6309] byte
  ror.idx      66 [6809,6309] byte
  asr.idx      67 [6809,6309] byte
  lsl.idx      68 [6809,6309] byte
  rol.idx      69 [6809,6309] byte
  dec.idx      6a [6809,6309] byte
  # tim.immidx 6b      [6309] byte
  inc.idx      6c [6809,6309] byte
  tst.idx      6d [6809,6309] byte
  jmp.idx      6e [6809,6309] code
  clr.idx      6f [6809,6309] byte

  neg.ext      70 [6809,6309] byte
  # oim.immext 71      [6309] byte
  # aim.immext 72      [6309] byte
  com.ext      73 [6809,6309] byte
  lsr.ext      74 [6809,6309] byte
  # eim.immext 75      [6309] byte
  ror.ext      76 [6809,6309] byte
  asr.ext      77 [6809,6309] byte
  lsl.ext      78 [6809,6309] byte
  rol.ext      79 [6809,6309] byte
  dec.ext      7a [6809,6309] byte
  # tim.immext 7b      [6309] byte
  inc.ext      7c [6809,6309] byte
  tst.ext      7d [6809,6309] byte
  jmp.ext      7e [6809,6309] code
  clr.ext      7f [6809,6309] byte

  suba.imm8    80 [6809,6309]
  cmpa.imm8    81 [6809,6309]
  sbca.imm8    82 [6809,6309]
  subd.imm16   83 [6809,6309]
  anda.imm8    84 [6809,6309]
  bita.imm8    85 [6809,6309]
  lda.imm8     86 [6809,6309]
  eora.imm8    88 [6809,6309]
  adca.imm8    89 [6809,6309]
  ora.imm8     8a [6809,6309]
  adda.imm8    8b [6809,6309]
  cmpx.imm16   8c [6809,6309]
  bsr.rel8     8d [6809,6309]
  ldx.imm16    8e [6809,6309]

  suba.dir     90 [6809,6309] byte
  cmpa.dir     91 [6809,6309] byte
  sbca.dir     92 [6809,6309] byte
  subd.dir     93 [6809,6309] byte
  anda.dir     94 [6809,6309] byte
  bita.dir     95 [6809,6309] byte
  lda.dir      96 [6809,6309] byte
  sta.dir      97 [6809,6309] byte
  eora.dir     98 [6809,6309] byte
  adca.dir     99 [6809,6309] byte
  ora.dir      9a [6809,6309] byte
  adda.dir     9b [6809,6309] byte
  cmpx.dir     9c [6809,6309] byte
  jsr.dir      9d [6809,6309] code
  ldx.dir      9e [6809,6309] byte
  stx.dir      9f [6809,6309] byte

  suba.idx     a0 [6809,6309] byte
  cmpa.idx     a1 [6809,6309] byte
  sbca.idx     a2 [6809,6309] byte
  subd.idx     a3 [6809,6309] byte
  anda.idx     a4 [6809,6309] byte
  bita.idx     a5 [6809,6309] byte
  lda.idx      a6 [6809,6309] byte
  sta.idx      a7 [6809,6309] byte
  eora.idx     a8 [6809,6309] byte
  adca.idx     a9 [6809,6309] byte
  ora.idx      aa [6809,6309] byte
  adda.idx     ab [6809,6309] byte
  cmpx.idx     ac [6809,6309] byte
  jsr.idx      ad [6809,6309] code
  ldx.idx      ae [6809,6309] byte
  stx.idx      af [6809,6309] byte

  suba.ext     b0 [6809,6309] byte
  cmpa.ext     b1 [6809,6309] byte
  sbca.ext     b2 [6809,6309] byte
  subd.ext     b3 [6809,6309] byte
  anda.ext     b4 [6809,6309] byte
  bita.ext     b5 [6809,6309] byte
  lda.ext      b6 [6809,6309] byte
  sta.ext      b7 [6809,6309] byte
  eora.ext     b8 [6809,6309] byte
  adca.ext     b9 [6809,6309] byte
  ora.ext      ba [6809,6309] byte
  adda.ext     bb [6809,6309] byte
  cmpx.ext     bc [6809,6309] byte
  jsr.ext      bd [6809,6309] code
  ldx.ext      be [6809,6309] byte
  stx.ext      bf [6809,6309] byte

  subb.imm8    c0 [6809,6309]
  cmpb.imm8    c1 [6809,6309]
  sbcb.imm8    c2 [6809,6309]
  addd.imm16   c3 [6809,6309]
  andb.imm8    c4 [6809,6309]
  bitb.imm8    c5 [6809,6309]
  ldb.imm8     c6 [6809,6309]
  eorb.imm8    c8 [6809,6309]
  adcb.imm8    c9 [6809,6309]
  orb.imm8     ca [6809,6309]
  addb.imm8    cb [6809,6309]
  ldd.imm16    cc [6809,6309]
  ldq.imm16    cd      [6309]
  ldu.imm16    ce [6809,6309]

  subb.dir     d0 [6809,6309] byte
  cmpb.dir     d1 [6809,6309] byte
  sbcb.dir     d2 [6809,6309] byte
  addd.dir     d3 [6809,6309] byte
  andb.dir     d4 [6809,6309] byte
  bitb.dir     d5 [6809,6309] byte
  ldb.dir      d6 [6809,6309] byte
  stb.dir      d7 [6809,6309] byte
  eorb.dir     d8 [6809,6309] byte
  adcb.dir     d9 [6809,6309] byte
  orb.dir      da [6809,6309] byte
  addb.dir     db [6809,6309] byte
  ldd.dir      dc [6809,6309] byte
  std.dir      dd [6809,6309] byte
  ldu.dir      de [6809,6309] byte
  stu.dir      df [6809,6309] byte

  subb.idx     e0 [6809,6309] byte
  cmpb.idx     e1 [6809,6309] byte
  sbcb.idx     e2 [6809,6309] byte
  addd.idx     e3 [6809,6309] byte
  andb.idx     e4 [6809,6309] byte
  bitb.idx     e5 [6809,6309] byte
  ldb.idx      e6 [6809,6309] byte
  stb.idx      e7 [6809,6309] byte
  eorb.idx     e8 [6809,6309] byte
  adcb.idx     e9 [6809,6309] byte
  orb.idx      ea [6809,6309] byte
  addb.idx     eb [6809,6309] byte
  ldd.idx      ec [6809,6309] byte
  std.idx      ed [6809,6309] byte
  ldu.idx      ee [6809,6309] byte
  stu.idx      ef [6809,6309] byte

  subb.ext     f0 [6809,6309] byte
  cmpb.ext     f1 [6809,6309] byte
  sbcb.ext     f2 [6809,6309] byte
  addd.ext     f3 [6809,6309] byte
  andb.ext     f4 [6809,6309] byte
  bitb.ext     f5 [6809,6309] byte
  ldb.ext      f6 [6809,6309] byte
  stb.ext      f7 [6809,6309] byte
  eorb.ext     f8 [6809,6309] byte
  adcb.ext     f9 [6809,6309] byte
  orb.ext      fa [6809,6309] byte
  addb.ext     fb [6809,6309] byte
  ldd.ext      fc [6809,6309] byte
  std.ext      fd [6809,6309] byte
  ldu.ext      fe [6809,6309] byte
  stu.ext      ff [6809,6309] byte


  # page 2

  lbrn.rel16 1021 [6809,6309]
  lbhi.rel16 1022 [6809,6309]
  lbls.rel16 1023 [6809,6309]
  lbcc.rel16 1024 [6809,6309]
  lbcs.rel16 1025 [6809,6309]
  lbne.rel16 1026 [6809,6309]
  lbeq.rel16 1027 [6809,6309]
  lbvc.rel16 1028 [6809,6309]
  lbvs.rel16 1029 [6809,6309]
  lbpl.rel16 102a [6809,6309]
  lbmi.rel16 102b [6809,6309]
  lbge.rel16 102c [6809,6309]
  lblt.rel16 102d [6809,6309]
  lbgt.rel16 102e [6809,6309]
  lble.rel16 102f [6809,6309]

  addr.imm8  1030      [6309]
  adcr.imm8  1031      [6309]
  subr.imm8  1032      [6309]
  sbcr.imm8  1033      [6309]
  andr.imm8  1034      [6309]
  orr.imm8   1035      [6309]
  eorr.imm8  1036      [6309]
  cmpr.imm8  1037      [6309]
  pshsw.imm8 1038      [6309]
  pulsw.imm8 1039      [6309]
  pshuw.imm8 103a      [6309]
  puluw.imm8 103b      [6309]
  swi2.imp   103f [6809,6309]

  negd.imp   1040      [6309]
  comd.imp   1043      [6309]
  lsrd.imp   1044      [6309]
  rord.imp   1046      [6309]
  asrd.imp   1047      [6309]
  lsld.imp   1048      [6309]
  rold.imp   1049      [6309]
  decd.imp   104a      [6309]
  incd.imp   104c      [6309]
  tstd.imp   104d      [6309]
  clrd.imp   104f      [6309]

  negw.imp   1050      [6309]
  comw.imp   1053      [6309]
  lsrw.imp   1054      [6309]
  rorw.imp   1056      [6309]
  asrw.imp   1057      [6309]
  lslw.imp   1058      [6309]
  decw.imp   105a      [6309]
  incw.imp   105c      [6309]
  tstw.imp   105d      [6309]
  clrw.imp   105f      [6309]

  subw.imm16 1080      [6309]
  cmpw.imm16 1081      [6309]
  sbcd.imm16 1082      [6309]
  cmpd.imm16 1083 [6809,6309]
  andd.imm16 1084      [6309]
  bitd.imm16 1085      [6309]
  ldw.imm16  1086      [6309]
  eord.imm16 1088      [6309]
  adcd.imm16 1089      [6309]
  ord.imm16  108a      [6309]
  addw.imm16 108b      [6309]
  cmpy.imm16 108c [6809,6309]
  ldy.imm16  108e [6809,6309]

  subw.dir   1090      [6309] byte
  cmpw.dir   1091      [6309] byte
  sbcd.dir   1092      [6309] byte
  cmpd.dir   1093 [6809,6309] byte
  andd.dir   1094      [6309] byte
  bitd.dir   1095      [6309] byte
  ldw.dir    1096      [6309] byte
  stw.dir    1097      [6309] byte
  eord.dir   1098      [6309] byte
  adcd.dir   1099      [6309] byte
  ord.dir    109a      [6309] byte
  addw.dir   109b      [6309] byte
  cmpy.dir   109c [6809,6309] byte
  ldy.dir    109e [6809,6309] byte
  sty.dir    109f [6809,6309] byte

  subw.idx   10a0      [6309] byte
  cmpw.idx   10a1      [6309] byte
  sbcd.idx   10a2      [6309] byte
  cmpd.idx   10a3 [6809,6309] byte
  andd.idx   10a4      [6309] byte
  bitd.idx   10a5      [6309] byte
  ldw.idx    10a6      [6309] byte
  stw.idx    10a7      [6309] byte
  eord.idx   10a8      [6309] byte
  adcd.idx   10a9      [6309] byte
  ord.idx    10aa      [6309] byte
  addw.idx   10ab      [6309] byte
  cmpy.idx   10ac [6809,6309] byte
  ldy.idx    10ae [6809,6309] byte
  sty.idx    10af [6809,6309] byte

  subw.ext   10b0      [6309] byte
  cmpw.ext   10b1      [6309] byte
  sbcd.ext   10b2      [6309] byte
  cmpd.ext   10b3 [6809,6309] byte
  andd.ext   10b4      [6309] byte
  bitd.ext   10b5      [6309] byte
  ldw.ext    10b6      [6309] byte
  stw.ext    10b7      [6309] byte
  eord.ext   10b8      [6309] byte
  adcd.ext   10b9      [6309] byte
  ord.ext    10ba      [6309] byte
  addw.ext   10bb      [6309] byte
  cmpy.ext   10bc [6809,6309] byte
  ldy.ext    10be [6809,6309] byte
  sty.ext    10bf [6809,6309] byte

  lds.imm16  10ce [6809,6309]

  ldq.dir    10dc      [6309] byte
  stq.dir    10dd      [6309] byte
  lds.dir    10de [6809,6309] byte
  sts.dir    10df [6809,6309] byte

  ldq.idx    10ec      [6309] byte
  stq.idx    10ed      [6309] byte
  lds.idx    10ee [6809,6309] byte
  sts.idx    10ef [6809,6309] byte

  ldq.ext    10fc      [6309] byte
  stq.ext    10fd      [6309] byte
  lds.ext    10fe [6809,6309] byte
  sts.ext    10ff [6809,6309] byte


  # page 3

  # band.bitdir  1130      [6309] byte
  # biand.bitdir 1131      [6309] byte
  # bor.bitdir   1132      [6309] byte
  # bior.bitdir  1133      [6309] byte
  # beor.bitdir  1134      [6309] byte
  # bieor.bitdir 1135      [6309] byte
  # ldbt.bitdir  1136      [6309] byte
  # stbt.bitdir  1137      [6309] byte
  tfmii.imm8 1138      [6309]
  tfmdd.imm8 1139      [6309]
  tfmik.imm8 113a      [6309]
  tfmki.imm8 113b      [6309]
  bitmd.imm8 113c      [6309]
  ldmd.imm8  113d      [6309]
  swi3.imp   113f [6809,6309]

  come.imp   1143      [6309]
  dece.imp   114a      [6309]
  ince.imp   114c      [6309]
  tste.imp   114d      [6309]
  clre.imp   114f      [6309]

  comf.imp   1153      [6309]
  decf.imp   115a      [6309]
  incf.imp   115c      [6309]
  tstf.imp   115d      [6309]
  clrf.imp   115f      [6309]

  sube.imm8  1180      [6309]
  cmpe.imm8  1181      [6309]
  cmpu.imm16 1183 [6809,6309]
  lde.imm8   1186      [6309]
  adde.imm8  118b      [6309]
  cmps.imm16 118c [6809,6309]
  divd.imm8  118d      [6309]
  divq.imm16 118e      [6309]
  muld.imm16 118f      [6309]

  sube.dir   1190      [6309] byte
  cmpe.dir   1191      [6309] byte
  cmpu.dir   1193 [6809,6309] byte
  lde.dir    1196      [6309] byte
  ste.dir    1197      [6309] byte
  adde.dir   119b      [6309] byte
  cmps.dir   119c [6809,6309] byte
  divd.dir   119d      [6309] byte
  divq.dir   119e      [6309] byte
  muld.dir   119f      [6309] byte

  sube.idx   11a0      [6309] byte
  cmpe.idx   11a1      [6309] byte
  cmpu.idx   11a3 [6809,6309] byte
  lde.idx    11a6      [6309] byte
  ste.idx    11a7      [6309] byte
  adde.idx   11ab      [6309] byte
  cmps.idx   11ac [6809,6309] byte
  divd.idx   11ad      [6309] byte
  divq.idx   11ae      [6309] byte
  muld.idx   11af      [6309] byte

  sube.ext   11b0      [6309] byte
  cmpe.ext   11b1      [6309] byte
  cmpu.ext   11b3 [6809,6309] byte
  lde.ext    11b6      [6309] byte
  ste.ext    11b7      [6309] byte
  adde.ext   11bb      [6309] byte
  cmps.ext   11bc [6809,6309] byte
  divd.ext   11bd      [6309] byte
  divq.ext   11be      [6309] byte
  muld.ext   11bf      [6309] byte

  subf.imm8  11c0      [6309]
  cmpf.imm8  11c1      [6309]
  ldf.imm8   11c6      [6309]
  addf.imm8  11cb      [6309]

  subf.dir   11d0      [6309] byte
  cmpf.dir   11d1      [6309] byte
  ldf.dir    11d6      [6309] byte
  stf.dir    11d7      [6309] byte
  addf.dir   11db      [6309] byte

  subf.idx   11e0      [6309] byte
  cmpf.idx   11e1      [6309] byte
  ldf.idx    11e6      [6309] byte
  stf.idx    11e7      [6309] byte
  addf.idx   11eb      [6309] byte

  subf.ext   11f0      [6309] byte
  cmpf.ext   11f1      [6309] byte
  ldf.ext    11f6      [6309] byte
  stf.ext    11f7      [6309] byte
  addf.ext   11fb      [6309] byte

  |]
  -- aliases
  [ ("asl", "lsl")
  , ("asla", "lsla")
  , ("aslb", "lslb")
  , ("asld", "lsld")
  , ("bhs", "bcc")
  , ("blo", "bcs")
  ]
