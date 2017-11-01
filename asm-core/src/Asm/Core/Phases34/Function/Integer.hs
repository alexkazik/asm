module Asm.Core.Phases34.Function.Integer
  ( iMinusUnary
  , iiMinusUnary
  , iMinus
  , iiMinus
  , iPlusUnary
  , iiPlusUnary
  , iPlus
  , iiPlus
  , iMul
  , iiMul
  , iDiv
  , iiDiv
  , iMod
  , iCom
  , iAnd
  , iOr
  , iXor
  , iShiftL
  , iiShiftL
  , iShiftR
  , iiShiftR
  , iRotateSection3
  , iRotateSection4
  , miPlusUnary
  , miRotateSection3
  , miRotateSection4
  , miAddDoNotCare
  , iSetCare
  , miEqual
  , miAnd
  , miOr
  , miXor
  , miCom
  , miShiftL
  , miShiftR
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Control.CompilerError
import           Asm.Core.Data.Ternary
import           Asm.Core.Phases.Data.CompilerState1234
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

iDiv :: CompilerState1234 m => Location -> Int64 -> Int64 -> m Int64
iDiv loc _ 0 = $throwFatalError [(loc, "divide by zero")]
iDiv _   a b = return $ div a b

iiDiv :: CompilerState1234 m => Location -> InfInt64 -> InfInt64 -> m InfInt64
iiDiv loc _            (InfInt64 0) = $throwFatalError [(loc, "divide by zero")]
iiDiv _   (InfInt64 a) (InfInt64 b)
  | a == minBound || a == maxBound || b == minBound || b == maxBound =
      if (a > 0 && b < 0) || (a < 0 && b > 0)
        then return minBound
        else return maxBound
  | otherwise = return $ fromInteger (div (toInteger a) (toInteger b))

iMod :: CompilerState1234 m => Location -> Int64 -> Int64 -> m Int64
iMod loc _ 0 = $throwFatalError [(loc, "divide by zero")]
iMod _   a b = return $ mod a b

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

iRotateSection3 :: CompilerState1234 m => Location -> Int64 -> Int -> Int -> m Int64
iRotateSection3 loc n c w = rotateSection loc 0 w n c

iRotateSection4 :: CompilerState1234 m => Location -> Int64 -> Int -> Int -> Int -> m Int64
iRotateSection4 loc n c w o = rotateSection loc o w n c


miPlusUnary :: TInt64 -> TInt64
miPlusUnary x = x

miRotateSection3 :: CompilerState1234 m => Location -> TInt64 -> Int -> Int -> m TInt64
miRotateSection3 loc n c w = do
  v' <- rotateSection loc 0 w v c
  m' <- rotateSection loc 0 w m c
  return (v' *| m')
  where
    (v, m) = tValueFillAndMask 0 n

miRotateSection4 :: CompilerState1234 m => Location -> TInt64 -> Int -> Int -> Int -> m TInt64
miRotateSection4 loc n c w o = do
  v' <- rotateSection loc o w v c
  m' <- rotateSection loc o w m c
  return (v' *| m')
  where
    (v, m) = tValueFillAndMask 0 n


rotateSection :: (CompilerState1234 m, Bits b, Num b) => Location -> Int -> Int -> b -> Int -> m b
rotateSection loc o w n c
  | o < 0 = $throwFatalError [(loc, "rotateSection: offset is negative")]
  | w < 1 = $throwFatalError [(loc, "rotateSection: width is zero or negative")]
  | maybe False (\x -> o+w > x) (bitSizeMaybe n)
      = $throwFatalError [(loc, "rotateSection: offset+width is bigger than the size")]
  | otherwise
      = return $
          n .&. complement mask .|.
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
