{-# LANGUAGE DeriveDataTypeable #-}

module Asm.Data.InfInt64
  ( InfInt64(..)
  ) where

import           Data.Data                    (Data)
import           Data.Int                     (Int64)
import           Data.Typeable                (Typeable)
import qualified Text.ParserCombinators.ReadP as RP
import qualified Text.Read                    as R

newtype InfInt64 = InfInt64 { unsafeFromInfInt64 :: Int64 }
  deriving (Bounded, Data, Eq, Ord, Typeable)

{-
  special cases:
    maxBound (0x7fffffffffffffff) is +inf
    minbound (-0x8000000000000000) is -inf
    the result of abs and negate of -0x7fffffffffffffff is +inf

  the following cases are usually undefined:
    +inf + -inf is +inf
    +inf - +inf is +inf
    ±inf * 0 is 0
-}

instance Num InfInt64 where
  (InfInt64 a) + (InfInt64 b)
    | a == maxBound || b == maxBound = maxBound
    | a == minBound || b == minBound = minBound
    | otherwise = fromInteger (toInteger a + toInteger b)
  (InfInt64 a) - (InfInt64 b)
    | a == maxBound || b == minBound = maxBound
    | a == minBound || b == maxBound = minBound
    | otherwise = fromInteger (toInteger a - toInteger b)
  (InfInt64 a) * (InfInt64 b)
    | a == 0 || b == 0 = 0
    | a == minBound || a == maxBound || b == minBound || b == maxBound =
      if (a > 0 && b < 0) || (a < 0 && b > 0)
        then minBound
        else maxBound
    | otherwise = fromInteger (toInteger a * toInteger b)
  abs (InfInt64 x)
    | x == minBound = maxBound
    | otherwise = InfInt64 (abs x)
  signum (InfInt64 x)
    = InfInt64 (signum x)
  fromInteger x
    | x <= toInteger (minBound :: Int64) = minBound
    | x >= toInteger (maxBound :: Int64) = maxBound
    | otherwise = InfInt64 (fromInteger x)
  negate (InfInt64 x)
    | x == maxBound = minBound
    | x == minBound = maxBound
    | otherwise = InfInt64 (negate x)

instance Show InfInt64 where
  show (InfInt64 x)
    | x == minBound = "-inf"
    | x == maxBound = "+inf"
    | otherwise = show x

instance Read InfInt64 where
  readPrec =
          R.lift (RP.string "+inf" >> return maxBound)
    R.+++ R.lift (RP.string "-inf" >> return minBound)
    R.+++ fmap InfInt64 R.readPrec
