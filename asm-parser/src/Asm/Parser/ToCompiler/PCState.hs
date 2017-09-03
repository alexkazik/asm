module Asm.Parser.ToCompiler.PCState
  ( PCStateData(..)
  , PCSM
  , pushPathPC
  , popPathPC
  , getPathPC
  ) where

import           Asm.Core.Prelude

import           Asm.Core.SourcePos

newtype PCStateData =
  PCSD
    { pcsPath :: Location
    }

-- the state monad it lives in
type PCSM = State PCStateData

-- function to change the state
pushPathPC :: SourcePos -> PCSM ()
pushPathPC name = state (\s -> ((), s{pcsPath=name : pcsPath s}))

popPathPC :: PCSM ()
popPathPC = state (\s -> ((), s{pcsPath=unsafeTail (pcsPath s)}))

getPathPC :: SourcePos -> PCSM Location
getPathPC loc = state (\s -> (loc : pcsPath s, s))
