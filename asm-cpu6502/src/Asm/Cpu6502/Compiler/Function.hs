module Asm.Cpu6502.Compiler.Function
  ( cpu6502SpecificFunctions
  , cpu6502SpecificMetaFunctions
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

import           Asm.Cpu6502.Data.CpuData6502
import           Asm.Cpu6502.Data.FunctionKey

--
-- functions for this cpu
--

cpu6502SpecificFunctions :: CSM34 m => [(FunctionKey, Function m Cpu6502)]
cpu6502SpecificFunctions =
  [ (fnConvertRel8, convertRel8C)
  , (fnCheckAddr8,  checkAddr8C)
  , (fnCheckAddr16, checkAddr16C)
  , (fnCheckJmpInd, checkJmpIndC)
  , (fnCheckLaxImm, checkLaxImmC)
  ]

cpu6502SpecificMetaFunctions :: [(FunctionKey, (FunctionKey -> Location -> [Expr4 c] -> CSM3 c (Expr4 c)))]
cpu6502SpecificMetaFunctions = []

--
-- implementations
--

convertRel8C :: CSM34 m => Function m Cpu6502
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

checkAddr16C :: CSM34 m => Function m Cpu6502
checkAddr16C loc [(KDData TDInt, E4ConstInt _ e)] = do
  when (e < 0 || e > 0xffff) $ $throwError [(loc, "address is out of the address range (0-0xffff)")]
  return (FnrResult (KDData TDInt, E4ConstInt loc e))
checkAddr16C _ [(KDData TDInt, _)] =
  return $ FnrUnchanged (KDData TDInt)
checkAddr16C _ _ = return FnrNoMatch

checkAddr8C :: CSM34 m => Function m Cpu6502
checkAddr8C loc [(KDData TDInt, E4ConstInt _ e)] = do
  when (e < 0 || e > 0xff) $ $throwError [(loc, "address is out of the address range (0-0xff)")]
  return (FnrResult (KDData TDInt, E4ConstInt loc e))
checkAddr8C _ [(KDData TDInt, _)] =
  return $ FnrUnchanged (KDData TDInt)
checkAddr8C _ _ = return FnrNoMatch

checkJmpIndC :: CSM34 m => Function m Cpu6502
checkJmpIndC loc [x@(KDData TDInt, E4ConstInt _ e)] = do
  when (e < 0 || e > 0xffff) $ $throwError [(loc, "address is out of the address range (0-0xffff)")]
  when ((e .&. 0x00ff) == 0x00ff) $ $throwError [(loc, "jmp (x); lo will be read from " ++ show e ++ " and high from " ++ show (e - 0xff))]
  return (FnrResult x)
checkJmpIndC _ [(KDData TDInt, _)] =
  return $ FnrUnchanged (KDData TDInt)
checkJmpIndC _ _ = return FnrNoMatch

checkLaxImmC :: CSM34 m => Function m Cpu6502
checkLaxImmC loc [x@(KDData TDInt, E4ConstInt _ e)] = do
  when (e /= 0) $ $throwError [(loc, "lax is only allowed with immediate 0 (or other addressing modes)")]
  return (FnrResult x)
checkLaxImmC _ [(KDData TDInt, _)] =
  return $ FnrUnchanged (KDData TDInt)
checkLaxImmC _ _ = return FnrNoMatch
