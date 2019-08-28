{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Asm.Core.Data.CpuData
  ( CpuData(..)
  , CS5Block
  ) where

import           Asm.Core.Prelude

import           Asm.Core.PrettyPrint

class
  ( Eq (CE4 c)
  , Eq (CS4 c)
  , Eq (CS5 c)
  , PrettySrc (CE4 c)
  , PrettySrc (CS4 c)
  , PrettySrc (CS5 c)
  ) => CpuData c where
    type CE12 c
    type CS12 c
    type CE3 c
    type CS3 c
    type CE4 c
    type CS4 c
    type CS5 c

type CS5Block c = [CS5 c]
