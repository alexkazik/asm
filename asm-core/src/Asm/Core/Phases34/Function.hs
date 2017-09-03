{-# LANGUAGE TemplateHaskell #-}

module Asm.Core.Phases34.Function
  ( generalFunctions
  , metaFunctions
  , functionKeyMap
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Data.Cpu
import           Asm.Core.Data.FunctionKey
import           Asm.Core.Phase3.Data.CompilerState3
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.Phases34.Data.CompilerState34
import           Asm.Core.Phases34.Data.Function
import           Asm.Core.Phases34.Function.Address
import           Asm.Core.Phases34.Function.Boolean
import           Asm.Core.Phases34.Function.Check
import           Asm.Core.Phases34.Function.Data
import           Asm.Core.Phases34.Function.Generate
import           Asm.Core.Phases34.Function.Integer
import           Asm.Core.SourcePos

generalFunctions :: (CSM34 m, Cpu c) => [(FunctionKey, Function m c)]
generalFunctions =
  -- Address
  [ (fnAddr,        addrC AddrCheckNone)
  , (fnAddrByte,    addrC AddrCheckByte)
  , (fnAddrCode,    addrC AddrCheckCode)
  , (fnBank,         bankC)
  , (fnSize,         sizeC)
  , (fnByte,         byteC)
  , (fnCode,         codeC)
  , (fnAs,           asC)
  -- Boolean
  , (opLT, addressRangeCompareLessC (<) False)
  , (opLE, addressRangeCompareLessC (<=) False)
  , (opGT, addressRangeCompareLessC (<=) True)
  , (opGE, addressRangeCompareLessC (<) True)
  , (opEQ, addressRangeCompareEqualC False)
  , (opNE, addressRangeCompareEqualC True)
  , (opEQ, $(toAsmFunction 'miEQ))
  , (opNE, $(toAsmFunction 'miNE))
  , (opEQ, $(toAsmFunction 'bEQ))
  , (opNE, $(toAsmFunction 'bNE))
  , (opNot, $(toAsmFunction 'not))
  , (opLAND, $(toAsmFunction '(&&)))
  , (opLOR, $(toAsmFunction '(||)))
  , (opLXOR, $(toAsmFunction 'bNE))
  -- Check
  , (fnCheckData8,         checkData8C)
  , (fnCheckData8Unsigned, checkData8UnsignedC)
  , (fnCheckData8Signed,   checkData8SignedC)
  , (fnCheckData8Both,     checkData8BothC)
  -- Data
  , (opConcat,   concatArrayC)
  , (fnFill, fillDataC)
  -- Integer
  , (opPLUS, $(toAsmFunctionNoWild 'iPlusUnary))
    , (opPLUS, $(toAsmFunction 'iiPlusUnary))
  , (opMINUS, $(toAsmFunctionNoWild 'iMinusUnary))
    , (opMINUS, $(toAsmFunction 'iiMinusUnary))
  , (opPLUS, $(toAsmFunctionNoWild 'iPlus))
    , (opPLUS, $(toAsmFunction 'iiPlus))
  , (opMINUS, addrDiffC)
    , (opMINUS, $(toAsmFunctionNoWild 'iMinus))
    , (opMINUS, $(toAsmFunction 'iiMinus))
  , (opMUL, $(toAsmFunctionNoWild 'iMul))
    , (opMUL, $(toAsmFunction 'iiMul))
  , (opDIV, $(toAsmFunctionNoWild 'iDiv))
    , (opDIV, $(toAsmFunction 'iiDiv))
  , (opMOD, $(toAsmFunction 'iMod))
  , (opCOM, $(toAsmFunction 'iCom))
  , (opAND, $(toAsmFunction 'iAnd))
  , (opOR, $(toAsmFunction 'iOr))
  , (opXOR, $(toAsmFunction 'iXor))
  , (opShiftL, $(toAsmFunctionNoWild 'iShiftL))
    , (opShiftL, $(toAsmFunction 'iiShiftL))
  , (opShiftR, $(toAsmFunctionNoWild 'iShiftR))
    , (opShiftR, $(toAsmFunction 'iiShiftR))
  , (fnRotate, $(toAsmFunction 'iRotateSection3))
  , (fnRotate, $(toAsmFunction 'iRotateSection4))
  , (opPLUS, $(toAsmFunction 'miPlusUnary))
  , (opCOM, $(toAsmFunction 'miCom))
  , (opAND, $(toAsmFunction 'miAnd))
  , (opOR, $(toAsmFunction 'miOr))
  , (opXOR, $(toAsmFunction 'miXor))
  , (opShiftL, $(toAsmFunction 'miShiftL))
  , (opShiftR, $(toAsmFunction 'miShiftR))
  , (fnRotate, $(toAsmFunction 'miRotateSection3))
  , (fnRotate, $(toAsmFunction 'miRotateSection4))
  , (opSetCare, $(toAsmFunction 'iSetCare))
  , (opAddDoNotCare, $(toAsmFunction 'miAddDoNotCare))
  ]

metaFunctions :: Cpu c => FunctionKeyMap (FunctionKey -> Location -> [Expr4 c] -> CSM3 c (Expr4 c))
metaFunctions = foldr (uncurry fkmInsert) fkmEmpty $
  [ (fnBank, bankMetaFunctionC)
  , (fnCheckData8, checkData8MetaFunctionC)
  , (fnFill, fillDataMetaFunctionC)
  ] ++ cpuSpecificMetaFunctions

functionKeyMap :: (CSM34 m, Cpu c) => FunctionKeyMap [Function m c]
functionKeyMap = foldr (\(n,f) -> fkmInsertWith (++) n [f]) fkmEmpty (generalFunctions ++ cpuSpecificFunctions)
