module Asm.Parser.Data.MagicValue
  ( MagicValue(..)
  ) where

import           Asm.Core.Prelude

newtype MagicValue = MagicValue { fromMagicValue :: Text }

instance IsString MagicValue where
  fromString = MagicValue . pack
