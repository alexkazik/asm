{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies #-}

module Asm.Cpu6809.Data.CpuSource6809  where

import           Asm.Core.Data.Cpu
import           Asm.Parser.ToCompiler.Stmt

import           Asm.Cpu6809.Data.Cpu6809     ()
import           Asm.Cpu6809.Data.CpuData6809

instance CpuSource Cpu6809 where
  type Source Cpu6809 = PStmtBlock6809
  fromSource = genericFromSource
