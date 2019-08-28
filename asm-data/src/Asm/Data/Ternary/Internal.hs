{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Asm.Data.Ternary.Internal
  ( Ternary(..)
  ) where

import           Data.Bits
import           Data.Data     (Data)
import           Data.Typeable (Typeable)

data Ternary b
  = !b :*|: !b
  deriving (Data, Eq, Typeable)
-- Invariant: All bits of the mask which are set (2nd argument) MUST be also set in the value (1st argument)
-- All functions here keep the invariant

-- Show instance
instance (Show b, Ord b, Bits b) => Show (Ternary b) where
  show (v :*|: m) =
    if case bitSizeMaybe m of
          Just n  -> n > 1 && testBit m (n - 1)
          Nothing -> m < zeroBits
      then show (v .&. complement m) ++ " *& " ++ show (complement m)
      else show (v .&. complement m) ++ " *| " ++ show m

-- Bits instance
instance Bits b => Bits (Ternary b) where
  (v1 :*|: m1) .&. (v2 :*|: m2) = (v1.&.v2) :*|: (v1.&.v2.&.(m1.|.m2))
  (v1 :*|: m1) .|. (v2 :*|: m2) = (v1.|.v2) :*|: ((m1.&.m2).|.(complement v1.&.m2).|.(m1.&. complement v2))
  (v1 :*|: m1) `xor` (v2 :*|: m2) = ((v1 `xor` v2) .|. (m1.|.m2)) :*|: (m1.|.m2)
  complement (v :*|: m) = (complement v .|. m) :*|: m
  (v :*|: m) `shift` i = (v `shift` i) :*|: (m `shift` i)
  (v :*|: m) `shiftL` i = (v `shiftL` i) :*|: (m `shiftL` i)
  (v :*|: m) `shiftR` i = (v `shiftR` i) :*|: (m `shiftR` i)
  (v :*|: m) `unsafeShiftL` i = (v `unsafeShiftL` i) :*|: (m `unsafeShiftL` i)
  (v :*|: m) `unsafeShiftR` i = (v `unsafeShiftR` i) :*|: (m `unsafeShiftR` i)
  (v :*|: m) `rotate` i = (v `rotate` i) :*|: (m `rotate` i)
  (v :*|: m) `rotateL` i = (v `rotateL` i) :*|: (m `rotateL` i)
  (v :*|: m) `rotateR` i = (v `rotateR` i) :*|: (m `rotateR` i)
  zeroBits = zeroBits :*|: zeroBits
  -- creates an number with all 0 and maybe except one 1 (no unknown)
  bit b = bit b :*|: zeroBits
  setBit (v :*|: m) b = setBit v b :*|: clearBit m b
  clearBit (v :*|: m) b = clearBit v b :*|: clearBit m b
  -- to set bits to unknown see (*-)
  complementBit vm@(v :*|: m) b
    | testBit m b = vm -- complement of unknown is unknown -> no change
    | otherwise = complementBit v b :*|: m
  bitSizeMaybe (v :*|: _) = bitSizeMaybe v
  isSigned (v :*|: _) = isSigned v
  -- this functions handle unknown as 0
  testBit (v :*|: m) = testBit (v .&. complement m)
  popCount (v :*|: m) = popCount (v .&. complement m)

-- FiniteBits instance
instance FiniteBits b => FiniteBits (Ternary b) where
  finiteBitSize (v :*|: _) = finiteBitSize v
  -- this functions handle unknown as 1
  countLeadingZeros (v :*|: _) = countLeadingZeros v
  countTrailingZeros (v :*|: _) = countTrailingZeros v

-- order: True > Unknown > False
-- WARING: the order is different for arbitrary sized integer types
instance (Ord b, Bits b) => Ord (Ternary b) where
  x1@(v1 :*|: m1) `compare` x2@(v2 :*|: m2) =
    if x1 == x2
      then EQ
      else
        case bitSizeMaybe v1 of
          Just x  -> cmp (x - 1)
          Nothing -> (mv1 `compare` mv2) `mappend` (v1 `compare` v2)
    where
      -- values with only 1 where true and 0 else (false: 0, 0; 0 xor 0 = 0) (unknown: 1, 1; 1 xor 1 = 0) (true: 1, 0; 1 xor 0 = 1)
      mv1 = m1 `xor` v1
      mv2 = m2 `xor` v2
      -- all bits are equal, so must the total then
      cmp (-1) = EQ
      -- compare one bit
      cmp x =
        (testBit mv1 x `compare` testBit mv2 x)
        `mappend`
        (testBit v1 x `compare` testBit v2 x)
        `mappend`
        (cmp (x-1))

instance Num b => Num (Ternary b) where
  fromInteger x = fromInteger x :*|: 0
  _ + _ = errorWithoutStackTrace "Asm.Data.Ternary.+: unsupported"
  _ - _ = errorWithoutStackTrace "Asm.Data.Ternary.-: unsupported"
  _ * _ = errorWithoutStackTrace "Asm.Data.Ternary.*: unsupported"
  negate _ = errorWithoutStackTrace "Asm.Data.Ternary.negate: unsupported"
  abs _ = errorWithoutStackTrace "Asm.Data.Ternary.abs: unsupported"
  signum _ = errorWithoutStackTrace "Asm.Data.Ternary.signum: unsupported"
