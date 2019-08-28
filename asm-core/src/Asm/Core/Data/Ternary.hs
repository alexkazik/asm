{-# LANGUAGE NoImplicitPrelude #-}

module Asm.Core.Data.Ternary
  ( module Asm.Data.Ternary
  , TInt64
  , isTInt64inRange
  ) where

import           Asm.Core.Prelude

import           Asm.Data.Ternary

type TInt64 = Ternary Int64

isTInt64inRange :: Int64 -> Int64 -> Int64 -> TInt64 -> Bool
isTInt64inRange lo hi msk j =
  let
    (i, m) = tValueFillAndMask 0 j
  in
    (i >= lo && i <= hi) && -- value is in range
    (m .|. msk == -1 || -- mask of all except range is don't care
    m .&. complement msk == 0) -- mask of all except range is care
