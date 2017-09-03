module Asm.Core.Phase2.Compiler
  ( compile2
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Data.Cpu
import           Asm.Core.Phase1.CompilerState1
import           Asm.Core.Phase2.CompilerState2
import           Asm.Core.Phase2.Data.Stmt2
import           Asm.Core.Phase2.LookupNames
import           Asm.Core.Phase2.RegisterTypes
import           Asm.Core.Phase3.Data.Stmt3

compile2 :: Cpu c => (Stmt2Block c, CompilerState1 c) -> (Stmt3Block c, CompilerState2 c)
compile2 (x1, s1) = runState (lookupNamesC x1 >>= registerTypesC) (initialState2 s1)
