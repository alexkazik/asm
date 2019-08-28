{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Demo.Multi.UnRLE
  ( moduleMultiUnRLE
  , unrle6809
  , unrle6502
  ) where

import           Asm.Cpu6502.Suffixed
import           Asm.Cpu6809.Suffixed

import           Demo.Output


unrle6809 :: Asm6809
unrle6809 = [asm6809|
    // This code can be relocated
    // X: ptr to input
    // Y: ptr to output
    // start with "jsr unrle.run"

  fill:
    adda 2
    ldb ,X+
  fill_loop:
    stb ,Y+
    deca
    bne fill_loop
    bra run

  copy:
    suba 0x7f -- subtract 0x80 to remove the topmost bit, add 1
  copy_loop:
    ldb ,X+
    stb ,Y+
    deca
    bne copy_loop

  run:
    lda ,X+
    bmi copy
    bne fill

  done:
    rts
  |]

demo6809 :: Asm6809
demo6809 = [asm6809|
    set meta.cpu = #6809
    pool out = 0x0000
    $block unrle6809 @out
  |]

unrle6309 :: Asm6809
unrle6309 = [asm6809|
    // This code can be relocated
    // E: must be zero!
    // X: ptr to input
    // Y: ptr to output
    // start with "jsr unrle.run"

  fill:
    addf 2
    tfm X, Y+
    leax 1, X -- X = X + 1
    bra run

  copy:
    subf 0x7f -- subtract 0x80 to remove the topmost bit, add 1
    tfm X+, Y+

  run:
    ldf ,X+
    bmi copy
    bne fill

  done:
    rts
  |]

demo6309 :: Asm6809
demo6309 = [asm6809|
    set meta.cpu = #6309
    pool out = 0x0000
    $block unrle6309 @out
  |]

unrle6502 :: LabelName -> Expr6502 -> Asm6502
unrle6502 source destination = [asm6502|
    // needs to be placed inside the zeropage
    // start with "jsr unrle.run"

    type struct {byte lo; byte hi} lohi

    inline lohi smc_source = addr($(source))
    inline lohi smc_destination = $(destination)

    inline byte smc_update_out = 0x??
    inline byte smc_update_in = 0x??

  fill:
    tax
    ldy 1
    sty smc_update_in
    lda [smc_source], y
    inx
    inx
    stx smc_update_out
  fill_loop:
    dex
    sta.abs @smc_destination, x
    bne fill_loop
    beq update

  copy:
    and 0x7f
    tay
    iny // add 1

    // keep in/out update
    sty smc_update_in
    sty smc_update_out

    // get a byte and write it
  copy_loop:
    lda [smc_source],y
    dey
    sta [smc_destination],y
    bne copy_loop

  update:
    clc
    lda smc_destination.lo
    adc.imm @smc_update_out
    sta smc_destination.lo
    bcc skip_uo
    inc smc_destination.hi
  skip_uo:

  update_in:
    sec -- always add the one control byte
    lda smc_source.lo
    adc.imm @smc_update_in
    sta smc_source.lo
    bcc skip_ui
    inc smc_source.hi
  skip_ui:

  run:
    lda.abs @smc_source
    bmi copy
    bne fill

  done:
    rts
  |]

demo6502 :: Asm6502
demo6502 = [asm6502|
    set meta.cpu = #6502
    pool out = 0x0000
    const byte[] source = [[0]] @out
    $block (unrle6502 "source" 1234) @out
  |]

moduleMultiUnRLE :: [ModuleOutput]
moduleMultiUnRLE =
  let
    cr6809 = compile6809 demo6809
    cr6309 = compile6809 demo6309
    cr6502 = compile6502 demo6502
  in
    [ moduleOutput
        "multi-unrle (6809)"
        cr6809
        [ ("unrle-6809.bin", poolDataToByteStringBuilder 0 (getOut cr6809)) ]
    , moduleOutput
        "multi-unrle (6309)"
        cr6309
        [ ("unrle-6309.bin", poolDataToByteStringBuilder 0 (getOut cr6309)) ]
    , moduleOutput
        "multi-unrle (6502)"
        cr6502
        [ ("unrle-6502.bin", poolDataToByteStringBuilder 0 (getOut cr6502)) ]
    ]
  where
    getOut cr =
      let
        Just (_poolStart, poolData) = lookup "out" (crPoolsWithData cr)
      in
        poolData
