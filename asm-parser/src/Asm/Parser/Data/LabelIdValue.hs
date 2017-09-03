module Asm.Parser.Data.LabelIdValue
  ( LabelIdValue(..)
  ) where

import           Asm.Core.Prelude

import           Asm.Parser.Data.Haskell

data LabelIdValue
  = LabelIdValueHaskell Haskell
  | LabelIdValue String
  deriving (Eq, Show, Typeable, Data)
