{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Asm.Cpu6809.Data.Cpu6809 where

import           Asm.Core.Data.Cpu
import           Asm.Parser.Parser.Tools

import Asm.Cpu6809.Data.CpuData6809

instance CpuParser Cpu6809 PStmtCpu6809 PExprCpu6809
instance Cpu Cpu6809
