{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Asm.Parser.Data.LabelIdValue
  ( LabelIdValue(..)
  ) where

import           Asm.Core.Prelude
import           Language.Haskell.TH (Exp)

data LabelIdValue
  = LabelIdValueHaskell Exp
  | LabelIdValue String
  deriving (Eq, Show, Typeable, Data)
