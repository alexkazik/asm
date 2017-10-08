{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Asm.Cpu6809.Data.OpCodes
  ( AddressMode(..)
  , module Asm.Cpu6809.Data.OpCodes
  ) where

import           Asm.Core.Prelude
import           Text.Heredoc

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

  neg.dir      00 [6809] byte
  com.dir      03 [6809] byte
  lsr.dir      04 [6809] byte
  ror.dir      06 [6809] byte
  asr.dir      07 [6809] byte
  lsl.dir      08 [6809] byte
  rol.dir      09 [6809] byte
  dec.dir      0a [6809] byte
  inc.dir      0c [6809] byte
  tst.dir      0d [6809] byte
  jmp.dir      0e [6809] code
  clr.dir      0f [6809] byte

  nop.imp      12 [6809]
  sync.imp     13 [6809]
  lbra.rel16   16 [6809]
  lbsr.rel16   17 [6809]
  daa.imp      19 [6809]
  orcc.imm8    1a [6809]
  andcc.imm8   1c [6809]
  sex.imp      1d [6809]
  exg.imm8     1e [6809]
  tfr.imm8     1f [6809]

  bra.rel8     20 [6809]
  brn.rel8     21 [6809]
  bhi.rel8     22 [6809]
  bls.rel8     23 [6809]
  bcc.rel8     24 [6809]
  bcs.rel8     25 [6809]
  bne.rel8     26 [6809]
  beq.rel8     27 [6809]
  bvc.rel8     28 [6809]
  bvs.rel8     29 [6809]
  bpl.rel8     2a [6809]
  bmi.rel8     2b [6809]
  bge.rel8     2c [6809]
  blt.rel8     2d [6809]
  bgt.rel8     2e [6809]
  ble.rel8     2f [6809]

  leax.idx     30 [6809] byte
  leay.idx     31 [6809] byte
  leas.idx     32 [6809] byte
  leau.idx     33 [6809] byte
  pshs.imm8    34 [6809]
  puls.imm8    35 [6809]
  pshu.imm8    36 [6809]
  pulu.imm8    37 [6809]
  rts.imp      39 [6809]
  abx.imp      3a [6809]
  rti.imp      3b [6809]
  cwai.imp     3c [6809]
  mul.imp      3d [6809]
  reset.imp    3e [6809] # the only undocumented opcode
  swi.imp      3f [6809]

  nega.imp     40 [6809]
  coma.imp     43 [6809]
  lsra.imp     44 [6809]
  rora.imp     46 [6809]
  asra.imp     47 [6809]
  lsla.imp     48 [6809]
  rola.imp     49 [6809]
  deca.imp     4a [6809]
  inca.imp     4c [6809]
  tsta.imp     4d [6809]
  clra.imp     4f [6809]

  negb.imp     50 [6809]
  comb.imp     53 [6809]
  lsrb.imp     54 [6809]
  rorb.imp     56 [6809]
  asrb.imp     57 [6809]
  lslb.imp     58 [6809]
  rolb.imp     59 [6809]
  decb.imp     5a [6809]
  incb.imp     5c [6809]
  tstb.imp     5d [6809]
  clrb.imp     5f [6809]

  neg.idx      60 [6809] byte
  com.idx      63 [6809] byte
  lsr.idx      64 [6809] byte
  ror.idx      66 [6809] byte
  asr.idx      67 [6809] byte
  lsl.idx      68 [6809] byte
  rol.idx      69 [6809] byte
  dec.idx      6a [6809] byte
  inc.idx      6c [6809] byte
  tst.idx      6d [6809] byte
  jmp.idx      6e [6809] code
  clr.idx      6f [6809] byte

  neg.ext      70 [6809] byte
  com.ext      73 [6809] byte
  lsr.ext      74 [6809] byte
  ror.ext      76 [6809] byte
  asr.ext      77 [6809] byte
  lsl.ext      78 [6809] byte
  rol.ext      79 [6809] byte
  dec.ext      7a [6809] byte
  inc.ext      7c [6809] byte
  tst.ext      7d [6809] byte
  jmp.ext      7e [6809] code
  clr.ext      7f [6809] byte

  suba.imm8    80 [6809]
  cmpa.imm8    81 [6809]
  sbca.imm8    82 [6809]
  subd.imm16   83 [6809]
  anda.imm8    84 [6809]
  bita.imm8    85 [6809]
  lda.imm8     86 [6809]
  eora.imm8    88 [6809]
  adca.imm8    89 [6809]
  ora.imm8     8a [6809]
  adda.imm8    8b [6809]
  cmpx.imm16   8c [6809]
  bsr.rel8     8d [6809]
  ldx.imm16    8e [6809]

  suba.dir     90 [6809] byte
  cmpa.dir     91 [6809] byte
  sbca.dir     92 [6809] byte
  subd.dir     93 [6809] byte
  anda.dir     94 [6809] byte
  bita.dir     95 [6809] byte
  lda.dir      96 [6809] byte
  sta.dir      97 [6809] byte
  eora.dir     98 [6809] byte
  adca.dir     99 [6809] byte
  ora.dir      9a [6809] byte
  adda.dir     9b [6809] byte
  cmpx.dir     9c [6809] byte
  jsr.dir      9d [6809] code
  ldx.dir      9e [6809] byte
  stx.dir      9f [6809] byte

  suba.idx     a0 [6809] byte
  cmpa.idx     a1 [6809] byte
  sbca.idx     a2 [6809] byte
  subd.idx     a3 [6809] byte
  anda.idx     a4 [6809] byte
  bita.idx     a5 [6809] byte
  lda.idx      a6 [6809] byte
  sta.idx      a7 [6809] byte
  eora.idx     a8 [6809] byte
  adca.idx     a9 [6809] byte
  ora.idx      aa [6809] byte
  adda.idx     ab [6809] byte
  cmpx.idx     ac [6809] byte
  jsr.idx      ad [6809] code
  ldx.idx      ae [6809] byte
  stx.idx      af [6809] byte

  suba.ext     b0 [6809] byte
  cmpa.ext     b1 [6809] byte
  sbca.ext     b2 [6809] byte
  subd.ext     b3 [6809] byte
  anda.ext     b4 [6809] byte
  bita.ext     b5 [6809] byte
  lda.ext      b6 [6809] byte
  sta.ext      b7 [6809] byte
  eora.ext     b8 [6809] byte
  adca.ext     b9 [6809] byte
  ora.ext      ba [6809] byte
  adda.ext     bb [6809] byte
  cmpx.ext     bc [6809] byte
  jsr.ext      bd [6809] code
  ldx.ext      be [6809] byte
  stx.ext      bf [6809] byte

  subb.imm8    c0 [6809]
  cmpb.imm8    c1 [6809]
  sbcb.imm8    c2 [6809]
  addd.imm16   c3 [6809]
  andb.imm8    c4 [6809]
  bitb.imm8    c5 [6809]
  ldb.imm8     c6 [6809]
  eorb.imm8    c8 [6809]
  adcb.imm8    c9 [6809]
  orb.imm8     ca [6809]
  addb.imm8    cb [6809]
  ldd.imm16    cc [6809]
  ldu.imm16    ce [6809]

  subb.dir     d0 [6809] byte
  cmpb.dir     d1 [6809] byte
  sbcb.dir     d2 [6809] byte
  addd.dir     d3 [6809] byte
  andb.dir     d4 [6809] byte
  bitb.dir     d5 [6809] byte
  ldb.dir      d6 [6809] byte
  stb.dir      d7 [6809] byte
  eorb.dir     d8 [6809] byte
  adcb.dir     d9 [6809] byte
  orb.dir      da [6809] byte
  addb.dir     db [6809] byte
  ldd.dir      dc [6809] byte
  std.dir      dd [6809] byte
  ldu.dir      de [6809] byte
  stu.dir      df [6809] byte

  subb.idx     e0 [6809] byte
  cmpb.idx     e1 [6809] byte
  sbcb.idx     e2 [6809] byte
  addd.idx     e3 [6809] byte
  andb.idx     e4 [6809] byte
  bitb.idx     e5 [6809] byte
  ldb.idx      e6 [6809] byte
  stb.idx      e7 [6809] byte
  eorb.idx     e8 [6809] byte
  adcb.idx     e9 [6809] byte
  orb.idx      ea [6809] byte
  addb.idx     eb [6809] byte
  ldd.idx      ec [6809] byte
  std.idx      ed [6809] byte
  ldu.idx      ee [6809] byte
  stu.idx      ef [6809] byte

  subb.ext     f0 [6809] byte
  cmpb.ext     f1 [6809] byte
  sbcb.ext     f2 [6809] byte
  addd.ext     f3 [6809] byte
  andb.ext     f4 [6809] byte
  bitb.ext     f5 [6809] byte
  ldb.ext      f6 [6809] byte
  stb.ext      f7 [6809] byte
  eorb.ext     f8 [6809] byte
  adcb.ext     f9 [6809] byte
  orb.ext      fa [6809] byte
  addb.ext     fb [6809] byte
  ldd.ext      fc [6809] byte
  std.ext      fd [6809] byte
  ldu.ext      fe [6809] byte
  stu.ext      ff [6809] byte


  # page 2

  lbrn.rel16 1021 [6809]
  lbhi.rel16 1022 [6809]
  lbls.rel16 1023 [6809]
  lbcc.rel16 1024 [6809]
  lbcs.rel16 1025 [6809]
  lbne.rel16 1026 [6809]
  lbeq.rel16 1027 [6809]
  lbvc.rel16 1028 [6809]
  lbvs.rel16 1029 [6809]
  lbpl.rel16 102a [6809]
  lbmi.rel16 102b [6809]
  lbge.rel16 102c [6809]
  lblt.rel16 102d [6809]
  lbgt.rel16 102e [6809]
  lble.rel16 102f [6809]

  swi2.imp   103f [6809]

  cmpd.imm16 1083 [6809]
  cmpy.imm16 108c [6809]
  ldy.imm16  108e [6809]

  cmpd.dir   1093 [6809] byte
  cmpy.dir   109c [6809] byte
  ldy.dir    109e [6809] byte
  sty.dir    109f [6809] byte

  cmpd.idx   10a3 [6809] byte
  cmpy.idx   10ac [6809] byte
  ldy.idx    10ae [6809] byte
  sty.idx    10af [6809] byte

  cmpd.ext   10b3 [6809] byte
  cmpy.ext   10bc [6809] byte
  ldy.ext    10be [6809] byte
  sty.ext    10bf [6809] byte

  lds.imm16  10ce [6809]

  lds.dir    10de [6809] byte
  sts.dir    10df [6809] byte

  lds.idx    10ee [6809] byte
  sts.idx    10ef [6809] byte

  lds.ext    10fe [6809] byte
  sts.ext    10ff [6809] byte


  # page 3

  swi3.imp   113f [6809]

  cmpu.imm16 1183 [6809]
  cmps.imm16 118c [6809]

  cmpu.dir   1193 [6809] byte
  cmps.dir   119c [6809] byte

  cmpu.idx   11a3 [6809] byte
  cmps.idx   11ac [6809] byte

  cmpu.ext   11b3 [6809] byte
  cmps.ext   11bc [6809] byte

  |]
  -- aliases
  [ ("asl", "lsl")
  , ("asla", "lsla")
  , ("aslb", "lslb")
  , ("bhs", "bcc")
  , ("blo", "bcs")
  ]
