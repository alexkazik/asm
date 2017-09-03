{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies #-}

module Asm.Cpu6502.Data.CpuSource6502  where

import           Asm.Core.Data.Cpu
import           Asm.Parser.ToCompiler.Stmt

import           Asm.Cpu6502.Data.Cpu6502     ()
import           Asm.Cpu6502.Data.CpuData6502

instance CpuSource Cpu6502 where
  type Source Cpu6502 = PStmtBlock6502
  fromSource = genericFromSource
