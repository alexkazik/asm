module Asm.Parser.Data.Int64Value
  ( Int64Value(..)
  ) where

import           Asm.Core.Prelude

import           Asm.Parser.Data.Haskell

data Int64Value
  = Int64ValueHaskell Haskell
  | Int64Value Int64
  deriving (Eq, Show, Typeable, Data)
