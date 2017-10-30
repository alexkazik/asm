module Asm.Parser.Data.Int64Value
  ( Int64Value(..)
  ) where

import           Asm.Core.Prelude
import           Language.Haskell.TH (Exp)

data Int64Value
  = Int64ValueHaskell Exp
  | Int64Value Int64
  deriving (Eq, Show, Typeable, Data)
