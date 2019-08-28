{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Asm.Cpu6809.Compiler.Function
  ( cpu6809SpecificFunctions
  , cpu6809SpecificMetaFunctions
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Control.CompilerError
import           Asm.Core.Data.KindDefinition
import           Asm.Core.Data.TypeDefinition
import           Asm.Core.Phase3.Data.CompilerState3
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.Phases34.Data.CompilerState34
import           Asm.Core.Phases34.Data.Function
import           Asm.Core.SourcePos
import           Asm.Data.InfInt64

import           Asm.Cpu6809.Data.CpuData6809
import           Asm.Cpu6809.Data.FunctionKey

checkRelIs8C :: CSM34 m => Function m Cpu6809
checkRelIs8C loc [(KDData TDInt, E4ConstInt _ e1), (KDData TDInt, E4ConstInt _ e2)] = do
  let
    d1 = (e1 - e2) .&. 0xffff
    d2 = if d1 >= 0x8000 then d1 - 0x10000 else d1
  return (FnrResult (KDData TDBool, E4ConstBool loc (e1 >= 0 && e1 <= 0xffff && e2 >= 0 && e2 <= 0xffff && d2 >= -128 && d2 <= 127)))
checkRelIs8C loc [(KDData TDInt, E4ConstInt _ e1), (KDData TDInt, E4RangedInt _ e2l e2h _ _)] =
  return $ checkRelIs8C' loc (InfInt64 e1) (InfInt64 e1) e2l e2h
checkRelIs8C loc [(KDData TDInt, E4RangedInt _ e1l e1h _ _), (KDData TDInt, E4ConstInt _ e2)] =
  return $ checkRelIs8C' loc e1l e1h (InfInt64 e2) (InfInt64 e2)
checkRelIs8C loc [(KDData TDInt, E4RangedInt _ e1l e1h _ _), (KDData TDInt, E4RangedInt _ e2l e2h _ _)] =
  return $ checkRelIs8C' loc e1l e1h e2l e2h
checkRelIs8C _ [(KDData TDInt, _), (KDData TDInt, _)] =
  return $ FnrUnchanged (KDData TDBool)
checkRelIs8C _ _ = return FnrNoMatch

checkRelIs8C' :: Location -> InfInt64 -> InfInt64 -> InfInt64 -> InfInt64 -> FunctionResult Cpu6809
checkRelIs8C' loc e1l e1h e2l e2h =
  if e1l < 0 || e1h > 0xffff || e2l < 0 || e2h > 0xffff
    then FnrUnchanged (KDData TDBool)
    else
      let
        d1 = abs (e1h - e2l) `max` abs (e2h - e1l)
        d2 = abs (e1h - e2l) `min` abs (e2h - e1l)
      in
        if d1 <= 127
          then FnrResult (KDData TDBool, E4ConstBool loc True)
          else if d2 > 128
            then FnrResult (KDData TDBool, E4ConstBool loc False)
            else FnrUnchanged (KDData TDBool)

convertRel8C :: CSM34 m => Function m Cpu6809
convertRel8C loc [(KDData TDInt, E4ConstInt _ e1), (KDData TDInt, E4ConstInt _ e2)] = do
  when (e1 < 0 || e1 > 0xffff) $ $throwError [(loc, "branch target is out of the address range (0-0xffff)")]
  when (e2 < 0 || e2 > 0xffff) $ $throwError [(loc, "branch source is out of the address range (0-0xffff)")]
  let
    d1 = (e1 - e2) .&. 0xffff
    d2 = if d1 >= 0x8000 then d1 - 0x10000 else d1
  when (d2 < -128 || d2 > 127) $ $throwError [(loc, "branch target is too far away (-128 .. 127 is possible, " ++ show d2 ++ " was requested)")]
  return (FnrResult (KDData TDInt, E4ConstInt loc d2))
convertRel8C _ [(KDData TDInt, _), (KDData TDInt, _)] =
  return $ FnrUnchanged (KDData TDInt)
convertRel8C _ _ = return FnrNoMatch

convertRel16C :: CSM34 m => Function m Cpu6809
convertRel16C loc [(KDData TDInt, E4ConstInt _ e1), (KDData TDInt, E4ConstInt _ e2)] = do
  when (e1 < 0 || e1 > 0xffff) $ $throwError [(loc, "branch target is out of the address range (0-0xffff)")]
  when (e2 < 0 || e2 > 0xffff) $ $throwError [(loc, "branch source is out of the address range (0-0xffff)")]
  let
    d1 = (e1 - e2) .&. 0xffff
  return (FnrResult (KDData TDInt, E4ConstInt loc d1))
convertRel16C _ [(KDData TDInt, _), (KDData TDInt, _)] =
  return $ FnrUnchanged (KDData TDInt)
convertRel16C _ _ = return FnrNoMatch

checkAddr16C :: CSM34 m => Function m Cpu6809
checkAddr16C loc [(KDData TDInt, E4ConstInt _ e)] = do
  when (e < 0 || e > 0xffff) $ $throwError [(loc, "address is out of the address range (0-0xffff)")]
  return (FnrResult (KDData TDInt, E4ConstInt loc e))
checkAddr16C _ [(KDData TDInt, _)] =
  return $ FnrUnchanged (KDData TDInt)
checkAddr16C _ _ = return FnrNoMatch

checkAddr8C :: CSM34 m => Function m Cpu6809
checkAddr8C loc [(KDData TDInt, E4ConstInt _ e)] = do
  when (e < 0 || e > 0xff) $ $throwError [(loc, "address is out of the address range (0-0xff)")]
  return (FnrResult (KDData TDInt, E4ConstInt loc e))
checkAddr8C _ [(KDData TDInt, _)] =
  return $ FnrUnchanged (KDData TDInt)
checkAddr8C _ _ = return FnrNoMatch

checkOffsetZeroC :: CSM34 m => Function m Cpu6809
checkOffsetZeroC loc [(KDData TDInt, E4ConstInt _ e)] = do
  when (e /= 0) $ $throwError [(loc, "the offset is not zero")]
  return (FnrResult (KDData TDBool, E4ConstBool loc True))
checkOffsetZeroC _ [(KDData TDInt, _)] =
  return $ FnrUnchanged (KDData TDBool)
checkOffsetZeroC _ _ = return FnrNoMatch

cpu6809SpecificFunctions :: CSM34 m => [(FunctionKey, Function m Cpu6809)]
cpu6809SpecificFunctions =
  [ (fnCheckRelIs8, checkRelIs8C)
  , (fnConvertRel8, convertRel8C)
  , (fnConvertRel16, convertRel16C)
  , (fnCheckAddr8,  checkAddr8C)
  , (fnCheckAddr16, checkAddr16C)
  , (fnCheckOffsetZero, checkOffsetZeroC)
  ]

cpu6809SpecificMetaFunctions :: [(FunctionKey, FunctionKey -> Location -> [Expr4 c] -> CSM3 c (Expr4 c))]
cpu6809SpecificMetaFunctions = []
