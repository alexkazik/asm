{-# LANGUAGE NoImplicitPrelude #-}

module Asm.Core.Phase1.Data.InitTree
  ( InitTree(..)
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Data.KindDefinition

newtype InitTree = I (Text, KindDefinition, [InitTree])
