{-# LANGUAGE FlexibleContexts #-}

module Asm.Core.Phases.Data.CompilerState1234
  ( CompilerState1234S(..)
  , CompilerState1234(..)
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Data.CompilerError
import           Asm.Data.Control.Monad.Error


class CompilerState1234S s where
  dumpStateS :: s -> LText

-- 1, 2, 3, 4
class MonadError CompilerError m => CompilerState1234 m where
  dumpStateC :: m LText
  default dumpStateC :: (MonadState s m, CompilerState1234S s) => m LText
  dumpStateC = dumpStateS <$> get
  setHasChangedC :: m ()
  setHasChangedC = return ()
