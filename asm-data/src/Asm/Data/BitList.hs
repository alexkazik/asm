{-# LANGUAGE DeriveDataTypeable #-}

module Asm.Data.BitList
 ( BitList(..)
 , PopulationResult(..)
 ) where

import           Data.Bits
import           Data.Data        (Data)
import           Data.Foldable
import           Data.Typeable    (Typeable)
import           Data.Word

import           Asm.Data.Word256

data PopulationResult
  = PrNone
  | PrSingle {-# UNPACK #-} !Int
  | PrMany
  | PrAll
  deriving (Data, Eq, Typeable)

class (FiniteBits b) => BitList b where
  fromList :: (Foldable t) => t Int -> b
  fromList = foldl' setBit zeroBits

  toList :: b -> [Int]
  toList x = filter (testBit x) [0 .. finiteBitSize x - 1]

  checkPopulation :: b -> PopulationResult
  checkPopulation v
    | v == zeroBits = PrNone
    | v == complement zeroBits = PrAll
    | otherwise =
        case Asm.Data.BitList.toList v of
          [x] -> PrSingle x
          _   -> PrMany

instance BitList Word

instance BitList Word64

instance BitList Word32

instance BitList Word16

instance BitList Word8 where
  checkPopulation 0   = PrNone
  checkPopulation 1   = PrSingle 0
  checkPopulation 2   = PrSingle 1
  checkPopulation 4   = PrSingle 2
  checkPopulation 8   = PrSingle 3
  checkPopulation 16  = PrSingle 4
  checkPopulation 32  = PrSingle 5
  checkPopulation 64  = PrSingle 6
  checkPopulation 128 = PrSingle 7
  checkPopulation 255 = PrAll
  checkPopulation _   = PrMany

instance BitList Word256 where
  checkPopulation (Word256 0 0 0 0)             = PrNone
  checkPopulation (Word256 0 0 0 d)             = checkPopulationWithOffset 0 d
  checkPopulation (Word256 0 0 c 0)             = checkPopulationWithOffset 64 c
  checkPopulation (Word256 0 b 0 0)             = checkPopulationWithOffset 128 b
  checkPopulation (Word256 a 0 0 0)             = checkPopulationWithOffset 192 a
  checkPopulation (Word256 (-1) (-1) (-1) (-1)) = PrAll
  checkPopulation _                             = PrMany

checkPopulationWithOffset :: BitList b => Int -> b -> PopulationResult
checkPopulationWithOffset o x =
  case checkPopulation x of
    PrSingle y -> PrSingle (o + y)
    _          -> PrMany
