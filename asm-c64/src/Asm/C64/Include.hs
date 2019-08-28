{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}

module Asm.C64.Include
  ( c64include
  ) where

import           Asm.Cpu6502

import           Asm.C64.Include.Basic
import           Asm.C64.Include.Hardware
import           Asm.C64.Include.Kernal

c64include :: Asm
c64include =
  [asm|
    -- structures only for internal use
    type struct { byte lo; byte hi; } _lohi
    type struct { byte x; byte y; } _xy
    -- cpu
    namespace cpu
      pointer byte ddr = 0x0000
      pointer byte port = 0x0001
      pointer byte[0x100] stack = 0x0100
      namespace vector
        pointer _lohi nmi = 0xfffa
        pointer _lohi reset = 0xfffc
        pointer _lohi irq = 0xfffe
      end
    end
    $(hardware)
    $(kernal)
    $(basic)
  |]
