module Asm.Core.Phase1.Compiler
  ( compile1
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Data.Cpu
import           Asm.Core.Phase1.CompilerState1
import           Asm.Core.Phase1.Data.Stmt1
import           Asm.Core.Phase1.RegisterNames
import           Asm.Core.Phase2.Data.Stmt2

compile1 :: Cpu c => Stmt1Block c -> (Stmt2Block c, CompilerState1 c, CompilerWriter1 c)
compile1 blk = runRWS (registerNamesC blk) () initialState1
