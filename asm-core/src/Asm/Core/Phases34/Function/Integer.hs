module Asm.Core.Phases34.Function.Integer where

import           Asm.Core.Prelude

import           Asm.Core.Data.Ternary
import           Asm.Core.SourcePos
import           Asm.Data.InfInt64

iMinusUnary :: Int64 -> Int64
iMinusUnary = negate

iiMinusUnary :: InfInt64 -> InfInt64
iiMinusUnary = negate

iMinus :: Int64 -> Int64 -> Int64
iMinus = (-)

iiMinus :: InfInt64 -> InfInt64 -> InfInt64
iiMinus = (-)

iPlusUnary :: Int64 -> Int64
iPlusUnary x = x

iiPlusUnary :: InfInt64 -> InfInt64
iiPlusUnary x = x

iPlus :: Int64 -> Int64 -> Int64
iPlus = (+)

iiPlus :: InfInt64 -> InfInt64 -> InfInt64
iiPlus = (+)

iMul :: Int64 -> Int64 -> Int64
iMul = (*)

iiMul :: InfInt64 -> InfInt64 -> InfInt64
iiMul = (*)

iDiv :: Int64 -> Int64 -> Int64
iDiv = div

iiDiv :: InfInt64 -> InfInt64 -> InfInt64
iiDiv (InfInt64 a) (InfInt64 b)
  | a == minBound || a == maxBound || b == minBound || b == maxBound =
      if (a > 0 && b < 0) || (a < 0 && b > 0)
        then minBound
        else maxBound
  | otherwise = fromInteger (div (toInteger a) (toInteger b))

iMod :: Int64 -> Int64 -> Int64
iMod = mod

iCom :: Int64 -> Int64
iCom = complement

iAnd :: Int64 -> Int64 -> Int64
iAnd = (.&.)

iOr :: Int64 -> Int64 -> Int64
iOr = (.|.)

iXor :: Int64 -> Int64 -> Int64
iXor = xor

iShiftL :: Int64 -> Int -> Int64
iShiftL = shiftL

iiShiftL :: InfInt64 -> Int -> InfInt64
iiShiftL (InfInt64 a) b
  | a == minBound = minBound
  | a == maxBound = maxBound
  | otherwise = fromInteger (shiftL (toInteger a) b)

iShiftR :: Int64 -> Int -> Int64
iShiftR = shiftR

iiShiftR :: InfInt64 -> Int -> InfInt64
iiShiftR (InfInt64 a) b
  | a == minBound = minBound
  | a == maxBound = maxBound
  | otherwise = fromInteger (shiftR (toInteger a) b)

iRotateSection3 :: Location -> Int64 -> Int -> Int -> Int64
iRotateSection3 loc n c w = rotateSection loc 0 w n c

iRotateSection4 :: Location -> Int64 -> Int -> Int -> Int -> Int64
iRotateSection4 loc n c w o = rotateSection loc o w n c


miPlusUnary :: TInt64 -> TInt64
miPlusUnary x = x

miRotateSection3 :: Location -> TInt64 -> Int -> Int -> TInt64
miRotateSection3 loc n c w = rotateSection loc 0 w v c *| rotateSection loc 0 w m c
  where
    (v, m) = tValueFillAndMask 0 n

miRotateSection4 :: Location -> TInt64 -> Int -> Int -> Int -> TInt64
miRotateSection4 loc n c w o = rotateSection loc o w v c *| rotateSection loc o w m c
  where
    (v, m) = tValueFillAndMask 0 n


rotateSection :: (Bits b, Num b) => Location -> Int -> Int -> b -> Int -> b
rotateSection loc o w n c
  | o < 0 = printError $ (loc, "rotateSection: offset is negative"):[sourcePos||]
  | w < 1 = printError $ (loc, "rotateSection: width is zero or negative"):[sourcePos||]
  | maybe False (\x -> o+w > x) (bitSizeMaybe n)
      = printError $ (loc, "rotateSection: offset+width is bigger than the size"):[sourcePos||]
  | otherwise
      = n .&. complement mask .|.
        (shiftL maskedN cc .|. shiftR maskedN (w - cc)) .&. mask
      where
        mask = shiftL (2 ^ w - 1) o
        maskedN = n .&. mask
        cc = c `mod` w

miAddDoNotCare :: TInt64 -> Int64 -> TInt64
miAddDoNotCare = (*-)

iSetCare :: Int64 -> Int64 -> TInt64
iSetCare = (*&)

miEqual :: TInt64 -> TInt64 -> Bool
miEqual = (==)

miAnd :: TInt64 -> TInt64 -> TInt64
miAnd = (.&.)

miOr :: TInt64 -> TInt64 -> TInt64
miOr = (.|.)

miXor :: TInt64 -> TInt64 -> TInt64
miXor = xor

miCom :: TInt64 -> TInt64
miCom = complement

miShiftL :: TInt64 -> Int -> TInt64
miShiftL = shiftL

miShiftR :: TInt64 -> Int -> TInt64
miShiftR = shiftR
