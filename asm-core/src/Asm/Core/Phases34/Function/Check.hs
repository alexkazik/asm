module Asm.Core.Phases34.Function.Check
  ( getCheck8C
  , checkData8MetaFunctionC
  , checkData8C
  , checkData8UnsignedC
  , checkData8SignedC
  , checkData8BothC
  , getCheck16C
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Data.Cpu
import           Asm.Core.Data.FunctionKey
import           Asm.Core.Data.KindDefinition
import           Asm.Core.Data.MetaKey
import           Asm.Core.Data.Ternary
import           Asm.Core.Data.TypeDefinition
import           Asm.Core.Phase3.CompilerState3
import           Asm.Core.Phase3.MetaData
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.Phases34.Data.Function
import           Asm.Core.PrettyPrint
import           Asm.Core.SourcePos
import           Asm.Data.InfInt64

getCheck8C :: Cpu c => Location -> [MetaKey] -> CSM3 c FunctionKey
getCheck8C loc metas = do
  cd' <- getMetaMagicMayC metas
  case cd' of
    Nothing              -> return fnCheckData8Unsigned
    Just ("unsigned", _) -> return fnCheckData8Unsigned
    Just ("signed", _)   -> return fnCheckData8Signed
    Just ("both", _)     -> return fnCheckData8Both
    Just ("off", _)      -> return fnCheckData8
    Just (x, cdLoc)      -> printErrorC $ (loc, "unknown check directive " ++ unpack x):(cdLoc,"defined at"):[sourcePos||]

checkData8MetaFunctionC :: Cpu c => FunctionKey -> Location -> [Expr4 c] -> CSM3 c (Expr4 c)
checkData8MetaFunctionC _f loc es = do
  fn <- getCheck8C loc [metaCheckData8, metaCheckData, metaCheck]
  return (E4Function loc fn es)

checkData8C :: (CSM34 m, Cpu c) => Function m c
checkData8C _ [x@(KDData TDInt, _)]       = return (FnrResult x)
checkData8C _ [x@(KDData TDMaskedInt, _)] = return (FnrResult x)
checkData8C _ [x@(KDData TDByte, _)]      = return (FnrResult x)
checkData8C loc es                        = printErrorC $ (loc, "unsupp mode: " ++ show (map showPrettySrc es)):[sourcePos||]

checkData8UnsignedC :: (CSM34 m, Cpu c) => Function m c
checkData8UnsignedC = checkDataAgainstC 0 255 0xff
checkData8SignedC :: (CSM34 m, Cpu c) => Function m c
checkData8SignedC = checkDataAgainstC (-128) 127 0xff
checkData8BothC :: (CSM34 m, Cpu c) => Function m c
checkData8BothC = checkDataAgainstC (-128) 255 0xff

checkDataAgainstC :: (CSM34 m, Cpu c) => Int64 -> Int64 -> Int64 -> Function m c
checkDataAgainstC lo hi _ loc [x@(KDData TDInt, E4ConstInt _ i)]
  | i >= lo && i <= hi = return (FnrResult x)
  | otherwise = printErrorC $ (loc, "integer out of range: " ++ show i):[sourcePos||]
checkDataAgainstC lo hi _ loc [(KDData TDInt, E4RangedInt _ l h _ e)]
  | l >= InfInt64 lo && h <= InfInt64 hi = return $ FnrResult (KDData TDInt, e)
  | l > InfInt64 hi || h < InfInt64 lo = printErrorC $ (loc, "integer out of range: " ++ show lo ++ ".." ++ show hi):[sourcePos||]
  | otherwise = return $ FnrUnchanged (KDData TDInt)
checkDataAgainstC _ _ _ _ [(KDData TDInt, _)] =
  return $ FnrUnchanged (KDData TDInt)
checkDataAgainstC lo hi msk loc [x@(KDData TDMaskedInt, E4ConstMaskedInt _ j)] =
  bool
    (printErrorC $ (loc, "maskedint out of range: " ++ show j):[sourcePos||])
    (return (FnrResult x))
    (isTInt64inRange lo hi msk j)
checkDataAgainstC _ _ _ _ [(KDData TDMaskedInt, _)] =
  return $ FnrUnchanged (KDData TDMaskedInt)
checkDataAgainstC _ _ _ _ [x@(KDData TDByte, _)] = return $ FnrResult x
checkDataAgainstC _ _ _ loc es = printErrorC $ (loc, "unsupp mode: " ++ show (map showPrettySrc es)):[sourcePos||]

getCheck16C :: Cpu c => Location -> [MetaKey] -> CSM3 c FunctionKey
getCheck16C _ _ = return fnCheckData8
