{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Asm.Cpu6502.Data.Cpu6502 where

import           Asm.Core.Data.Cpu
import           Asm.Parser.Parser.Tools

import Asm.Cpu6502.Data.CpuData6502

instance CpuParser Cpu6502 PStmtCpu6502 PExprCpu6502
instance Cpu Cpu6502
