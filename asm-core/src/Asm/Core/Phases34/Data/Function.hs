{-# LANGUAGE NoImplicitPrelude #-}

module Asm.Core.Phases34.Data.Function
  ( Function
  , FunctionResult(..)
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Data.KindDefinition
import           Asm.Core.Data.Reference
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.SourcePos
import           Asm.Data.InfInt64

data FunctionResult c
  = FnrResult (KindDefinition, Expr4 c)
  | FnrRangedInt InfInt64 InfInt64 (Maybe (Reference, Int64))
  | FnrUnchanged KindDefinition
  | FnrNoMatch
  deriving Show

type Function m c = Location -> [(KindDefinition, Expr4 c)] -> m (FunctionResult c)
