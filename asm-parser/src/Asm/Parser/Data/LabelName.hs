{-# LANGUAGE NoImplicitPrelude #-}

module Asm.Parser.Data.LabelName
  ( LabelName(..)
  ) where

import           Asm.Core.Prelude

newtype LabelName = LabelName { fromLabelName :: String }

instance IsString LabelName where
  fromString = LabelName
