{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Asm.Cpu6502.Data.OpCodes
  ( AddressMode(..)
  , IndexMode(..)
  , module Asm.Cpu6502.Data.OpCodes
  ) where

import           Asm.Core.Prelude
import           Text.Heredoc

import           Asm.Cpu6502.OpCodes.Generator

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

  opcodes :: IntMap (IntMap (Map IndexMode (Map AddressMode (Word8, Maybe FunctionKey, Maybe FunctionKey))))
    the first IntMap is an unpacked Map CpuVersion
    the second IntMap is an unpacked Map Operator
-}

mkOpCodes
  [here|
    # all relative
    bpl.rel   10 [6502,6502i,65c02]
    bmi.rel   30 [6502,6502i,65c02]
    bvc.rel   50 [6502,6502i,65c02]
    bvs.rel   70 [6502,6502i,65c02]
    bcc.rel   90 [6502,6502i,65c02]
    bcs.rel   b0 [6502,6502i,65c02]
    bne.rel   d0 [6502,6502i,65c02]
    beq.rel   f0 [6502,6502i,65c02]

    # jmp/jsr
    jsr.abs   20 [6502,6502i,65c02] code
    jmp.abs   4c [6502,6502i,65c02] code
    jmp.abs-i 6c [6502,6502i]       byte fnCheckJmpInd
    jmp.abs-i 6c            [65c02] byte

    # all implied
    brk.imp   00 [6502,6502i,65c02]
    php.imp   08 [6502,6502i,65c02]
    asl.imp   0a [6502,6502i,65c02]
    clc.imp   18 [6502,6502i,65c02]
    plp.imp   28 [6502,6502i,65c02]
    rol.imp   2a [6502,6502i,65c02]
    sec.imp   38 [6502,6502i,65c02]
    rti.imp   40 [6502,6502i,65c02]
    pha.imp   48 [6502,6502i,65c02]
    lsr.imp   4a [6502,6502i,65c02]
    cli.imp   58 [6502,6502i,65c02]
    rts.imp   60 [6502,6502i,65c02]
    pla.imp   68 [6502,6502i,65c02]
    ror.imp   6a [6502,6502i,65c02]
    sei.imp   78 [6502,6502i,65c02]
    dey.imp   88 [6502,6502i,65c02]
    txa.imp   8a [6502,6502i,65c02]
    tya.imp   98 [6502,6502i,65c02]
    txs.imp   9a [6502,6502i,65c02]
    tay.imp   a8 [6502,6502i,65c02]
    tax.imp   aa [6502,6502i,65c02]
    clv.imp   b8 [6502,6502i,65c02]
    tsx.imp   ba [6502,6502i,65c02]
    iny.imp   c8 [6502,6502i,65c02]
    dex.imp   ca [6502,6502i,65c02]
    cld.imp   d8 [6502,6502i,65c02]
    inx.imp   e8 [6502,6502i,65c02]
    nop.imp   ea [6502,6502i,65c02]
    sed.imp   f8 [6502,6502i,65c02]

    # all others (sorted by bits 0b---XXX--)
    ldy.imm   a0 [6502,6502i,65c02]
    ldx.imm   a2 [6502,6502i,65c02]
    cpy.imm   c0 [6502,6502i,65c02]
    cpx.imm   e0 [6502,6502i,65c02]
    asl.zp    06 [6502,6502i,65c02] byte
    bit.zp    24 [6502,6502i,65c02] byte
    rol.zp    26 [6502,6502i,65c02] byte
    lsr.zp    46 [6502,6502i,65c02] byte
    ror.zp    66 [6502,6502i,65c02] byte
    sty.zp    84 [6502,6502i,65c02] byte
    stx.zp    86 [6502,6502i,65c02] byte
    ldy.zp    a4 [6502,6502i,65c02] byte
    ldx.zp    a6 [6502,6502i,65c02] byte
    cpy.zp    c4 [6502,6502i,65c02] byte
    dec.zp    c6 [6502,6502i,65c02] byte
    cpx.zp    e4 [6502,6502i,65c02] byte
    inc.zp    e6 [6502,6502i,65c02] byte
    asl.abs   0e [6502,6502i,65c02] byte
    bit.abs   2c [6502,6502i,65c02] byte
    rol.abs   2e [6502,6502i,65c02] byte
    lsr.abs   4e [6502,6502i,65c02] byte
    ror.abs   6e [6502,6502i,65c02] byte
    sty.abs   8c [6502,6502i,65c02] byte
    stx.abs   8e [6502,6502i,65c02] byte
    ldy.abs   ac [6502,6502i,65c02] byte
    ldx.abs   ae [6502,6502i,65c02] byte
    cpy.abs   cc [6502,6502i,65c02] byte
    dec.abs   ce [6502,6502i,65c02] byte
    cpx.abs   ec [6502,6502i,65c02] byte
    inc.abs   ee [6502,6502i,65c02] byte
    asl.zp-x  16 [6502,6502i,65c02] byte
    rol.zp-x  36 [6502,6502i,65c02] byte
    lsr.zp-x  56 [6502,6502i,65c02] byte
    ror.zp-x  76 [6502,6502i,65c02] byte
    sty.zp-x  94 [6502,6502i,65c02] byte
    stx.zp-y  96 [6502,6502i,65c02] byte
    ldy.zp-x  b4 [6502,6502i,65c02] byte
    ldx.zp-y  b6 [6502,6502i,65c02] byte
    dec.zp-x  d6 [6502,6502i,65c02] byte
    inc.zp-x  f6 [6502,6502i,65c02] byte
    asl.abs-x 1e [6502,6502i,65c02] byte
    rol.abs-x 3e [6502,6502i,65c02] byte
    lsr.abs-x 5e [6502,6502i,65c02] byte
    ror.abs-x 7e [6502,6502i,65c02] byte
    ldy.abs-x bc [6502,6502i,65c02] byte
    ldx.abs-y be [6502,6502i,65c02] byte
    dec.abs-x de [6502,6502i,65c02] byte
    inc.abs-x fe [6502,6502i,65c02] byte
    ora.zp-xi 01 [6502,6502i,65c02] byte
    and.zp-xi 21 [6502,6502i,65c02] byte
    eor.zp-xi 41 [6502,6502i,65c02] byte
    adc.zp-xi 61 [6502,6502i,65c02] byte
    sta.zp-xi 81 [6502,6502i,65c02] byte
    lda.zp-xi a1 [6502,6502i,65c02] byte
    cmp.zp-xi c1 [6502,6502i,65c02] byte
    sbc.zp-xi e1 [6502,6502i,65c02] byte
    ora.zp    05 [6502,6502i,65c02] byte
    and.zp    25 [6502,6502i,65c02] byte
    eor.zp    45 [6502,6502i,65c02] byte
    adc.zp    65 [6502,6502i,65c02] byte
    sta.zp    85 [6502,6502i,65c02] byte
    lda.zp    a5 [6502,6502i,65c02] byte
    cmp.zp    c5 [6502,6502i,65c02] byte
    sbc.zp    e5 [6502,6502i,65c02] byte
    ora.imm   09 [6502,6502i,65c02]
    and.imm   29 [6502,6502i,65c02]
    eor.imm   49 [6502,6502i,65c02]
    adc.imm   69 [6502,6502i,65c02]
    lda.imm   a9 [6502,6502i,65c02]
    cmp.imm   c9 [6502,6502i,65c02]
    sbc.imm   e9 [6502,6502i,65c02]
    ora.abs   0d [6502,6502i,65c02] byte
    and.abs   2d [6502,6502i,65c02] byte
    eor.abs   4d [6502,6502i,65c02] byte
    adc.abs   6d [6502,6502i,65c02] byte
    sta.abs   8d [6502,6502i,65c02] byte
    lda.abs   ad [6502,6502i,65c02] byte
    cmp.abs   cd [6502,6502i,65c02] byte
    sbc.abs   ed [6502,6502i,65c02] byte
    ora.zp-iy 11 [6502,6502i,65c02] byte
    and.zp-iy 31 [6502,6502i,65c02] byte
    eor.zp-iy 51 [6502,6502i,65c02] byte
    adc.zp-iy 71 [6502,6502i,65c02] byte
    sta.zp-iy 91 [6502,6502i,65c02] byte
    lda.zp-iy b1 [6502,6502i,65c02] byte
    cmp.zp-iy d1 [6502,6502i,65c02] byte
    sbc.zp-iy f1 [6502,6502i,65c02] byte
    ora.zp-x  15 [6502,6502i,65c02] byte
    and.zp-x  35 [6502,6502i,65c02] byte
    eor.zp-x  55 [6502,6502i,65c02] byte
    adc.zp-x  75 [6502,6502i,65c02] byte
    sta.zp-x  95 [6502,6502i,65c02] byte
    lda.zp-x  b5 [6502,6502i,65c02] byte
    cmp.zp-x  d5 [6502,6502i,65c02] byte
    sbc.zp-x  f5 [6502,6502i,65c02] byte
    ora.abs-y 19 [6502,6502i,65c02] byte
    and.abs-y 39 [6502,6502i,65c02] byte
    eor.abs-y 59 [6502,6502i,65c02] byte
    adc.abs-y 79 [6502,6502i,65c02] byte
    sta.abs-y 99 [6502,6502i,65c02] byte
    lda.abs-y b9 [6502,6502i,65c02] byte
    cmp.abs-y d9 [6502,6502i,65c02] byte
    sbc.abs-y f9 [6502,6502i,65c02] byte
    ora.abs-x 1d [6502,6502i,65c02] byte
    and.abs-x 3d [6502,6502i,65c02] byte
    eor.abs-x 5d [6502,6502i,65c02] byte
    adc.abs-x 7d [6502,6502i,65c02] byte
    sta.abs-x 9d [6502,6502i,65c02] byte
    lda.abs-x bd [6502,6502i,65c02] byte
    cmp.abs-x dd [6502,6502i,65c02] byte
    sbc.abs-x fd [6502,6502i,65c02] byte

    # common operands not available on 6502
    nop.imm   42            [65c02]
    nop.imm   80      [6502i]
    ldn.zp    44      [6502i,65c02] byte
    ldn.abs   0c      [6502i]       byte
    ldn.zp-x  f4      [6502i,65c02] byte
    ldn.abs-x fc      [6502i,65c02] byte

    # operands only available on 65c02
    nop1.imp  03 [65c02]
    tsb.zp    04 [65c02] byte
    tsb.abs   0c [65c02] byte
    trb.zp    14 [65c02] byte
    inc.imp   1a [65c02]
    trb.abs   1c [65c02] byte
    bit.zp-x  34 [65c02] byte
    dec.imp   3a [65c02]
    bit.abs-x 3c [65c02] byte
    phy.imp   5a [65c02]
    stz.zp    64 [65c02] byte
    stz.zp-x  74 [65c02] byte
    ply.imp   7a [65c02]
    bra.rel   80 [65c02]
    bit.imm   89 [65c02]
    stz.abs   9c [65c02] byte
    stz.abs-x 9e [65c02] byte
    phx.imp   da [65c02]
    plx.imp   fa [65c02]

    # operands only available on 6502i
    jam.imp   02 [6502i]

    slo.zp-xi 03 [6502i] byte
    rla.zp-xi 23 [6502i] byte
    sre.zp-xi 43 [6502i] byte
    rra.zp-xi 63 [6502i] byte
    sax.zp-xi 83 [6502i] byte
    lax.zp-xi a3 [6502i] byte
    dcp.zp-xi c3 [6502i] byte
    isc.zp-xi e3 [6502i] byte

    slo.zp-iy 13 [6502i] byte
    rla.zp-iy 33 [6502i] byte
    sre.zp-iy 53 [6502i] byte
    rra.zp-iy 73 [6502i] byte
    lax.zp-iy b3 [6502i] byte
    dcp.zp-iy d3 [6502i] byte
    isc.zp-iy f3 [6502i] byte

    slo.zp    07 [6502i] byte
    rla.zp    27 [6502i] byte
    sre.zp    47 [6502i] byte
    rra.zp    67 [6502i] byte
    sax.zp    87 [6502i] byte
    lax.zp    a7 [6502i] byte
    dcp.zp    c7 [6502i] byte
    isc.zp    e7 [6502i] byte

    slo.zp-x  17 [6502i] byte
    rla.zp-x  37 [6502i] byte
    sre.zp-x  57 [6502i] byte
    rra.zp-x  77 [6502i] byte
    dcp.zp-x  d7 [6502i] byte
    isc.zp-x  f7 [6502i] byte

    sax.zp-y  97 [6502i] byte
    lax.zp-y  b7 [6502i] byte

    anc.imm   0b [6502i]
    alr.imm   4b [6502i]
    arr.imm   6b [6502i]
    lax.imm   ab [6502i] -    fnCheckLaxImm
    sbx.imm   cb [6502i]

    slo.abs-y 1b [6502i] byte
    rla.abs-y 3b [6502i] byte
    sre.abs-y 5b [6502i] byte
    rra.abs-y 7b [6502i] byte
    lax.abs-y bf [6502i] byte
    dcp.abs-y db [6502i] byte
    isc.abs-y fb [6502i] byte

    slo.abs   0f [6502i] byte
    rla.abs   2f [6502i] byte
    sre.abs   4f [6502i] byte
    rra.abs   6f [6502i] byte
    sax.abs   8f [6502i] byte
    lax.abs   af [6502i] byte
    dcp.abs   cf [6502i] byte
    isc.abs   ef [6502i] byte

    slo.abs-x 1f [6502i] byte
    rla.abs-x 3f [6502i] byte
    sre.abs-x 5f [6502i] byte
    rra.abs-x 7f [6502i] byte
    dcp.abs-x df [6502i] byte
    isc.abs-x ff [6502i] byte
  |]
  -- aliases
  [ ("lsl", "asl")
  , ("bhs", "bcc")
  , ("blo", "bcs")
  ]

minCpuVariant :: CpuVariant -> CpuVariant -> CpuVariant
a `minCpuVariant` b
  | a == cpuVariant6502i && b == cpuVariant6502i = cpuVariant6502i
  | a == cpuVariant65c02 && b == cpuVariant65c02 = cpuVariant65c02
  | otherwise = cpuVariant6502
