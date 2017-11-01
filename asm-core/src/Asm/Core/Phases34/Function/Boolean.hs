module Asm.Core.Phases34.Function.Boolean
  ( iLT
  , iLE
  , iGT
  , iGE
  , iEQ
  , iNE
  , miEQ
  , miNE
  , bEQ
  , bNE
  , addressRangeCompareLessC
  , addressRangeCompareEqualC
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Data.Cpu
import           Asm.Core.Data.KindDefinition
import           Asm.Core.Data.Reference
import           Asm.Core.Data.Ternary
import           Asm.Core.Data.TypeDefinition
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.Phases34.Data.CompilerState34
import           Asm.Core.Phases34.Data.Function
import           Asm.Data.InfInt64

iLT :: Int64 -> Int64 -> Bool
iLT = (<)

iLE :: Int64 -> Int64 -> Bool
iLE = (<=)

iGT :: Int64 -> Int64 -> Bool
iGT = (>)

iGE :: Int64 -> Int64 -> Bool
iGE = (>=)

iEQ :: Int64 -> Int64 -> Bool
iEQ = (==)

iNE :: Int64 -> Int64 -> Bool
iNE = (/=)

miEQ :: TInt64 -> TInt64 -> Bool
miEQ = (==)

miNE :: TInt64 -> TInt64 -> Bool
miNE = (/=)

bEQ :: Bool -> Bool -> Bool
bEQ = (==)

bNE :: Bool -> Bool -> Bool
bNE = (/=)

addressRangeCompareLessC :: (CSM34 m, Cpu c) => (InfInt64 -> InfInt64 -> Bool) -> Bool -> Function m c
addressRangeCompareLessC cmp swapArgs loc [(KDData TDInt, el), (KDData TDInt, er)] =
  let
    el' = toRange el
    er' = toRange er
  in
    case bool (el', er') (er', el') swapArgs of
      (Just (lLo, lHi, lNo), Just (rLo, rHi, rNo)) ->
        if | cmp lHi rLo -> -- leftHi <= rightLo (or <)
               return $ FnrResult (KDData TDBool, E4ConstBool loc True)
           | not (cmp lLo rHi) -> -- leftLo > rightHi (or >=)
               return $ FnrResult (KDData TDBool, E4ConstBool loc False)
           | otherwise ->
               case (lNo, rNo) of
                 (Just (lN, lO), Just (rN, rO)) ->
                   case getRelativeOffset lN rN of
                     Just rel -> return $ FnrResult (KDData TDBool, E4ConstBool loc $ cmp (fromIntegral $ rel + lO - rO) 0)
                     _        -> return $ FnrUnchanged (KDData TDBool)
                 _ ->
                   return $ FnrUnchanged (KDData TDBool)
      _ -> return $ FnrUnchanged (KDData TDBool)
  where
    toRange (E4RangedInt _ l h no _) = Just (l, h, no)
    toRange (E4ConstInt _ i)         = Just (fromIntegral i, fromIntegral i, Nothing)
    toRange _                        = Nothing
addressRangeCompareLessC _ _ _ _ = return FnrNoMatch

addressRangeCompareEqualC :: (CSM34 m, Cpu c) => Bool -> Function m c
addressRangeCompareEqualC neg loc [(KDData TDInt, E4ConstInt _ l), (KDData TDInt, E4ConstInt _ r)] =
  return $ FnrResult (KDData TDBool, E4ConstBool loc $ neg `xor` (l == r))
addressRangeCompareEqualC neg loc [(KDData TDInt, el), (KDData TDInt, er)] =
  case (toRange el, toRange er) of
    (Just (lLo, lHi, lNo), Just (rLo, rHi, rNo)) ->
      if not (lLo <= rHi && rLo <= lHi) -- ranges do not overlap
        then
          return $ FnrResult (KDData TDBool, E4ConstBool loc neg)
        else
          case (lNo, rNo) of
            (Just (lN, lO), Just (rN, rO)) ->
              case getRelativeOffset lN rN of
                Just rel -> return $ FnrResult (KDData TDBool, E4ConstBool loc $ neg `xor` (rel + lO - rO == 0))
                _        -> return $ FnrUnchanged (KDData TDBool)
            _ ->
              return $ FnrUnchanged (KDData TDBool)
    _ -> return $ FnrUnchanged (KDData TDBool)
  where
    toRange (E4RangedInt _ l h no _) = Just (l, h, no)
    toRange (E4ConstInt _ i)         = Just (fromIntegral i, fromIntegral i, Nothing)
    toRange _                        = Nothing
addressRangeCompareEqualC _ _ _ = return FnrNoMatch

getRelativeOffset :: Reference -> Reference -> Maybe Int64
-- TODO: check if both are in the same pool(data) and return relation then
-- TODO: move to somewhere else
getRelativeOffset _ _ = Nothing
