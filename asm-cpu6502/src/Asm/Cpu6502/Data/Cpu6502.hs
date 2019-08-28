{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Asm.Cpu6502.Data.Cpu6502
  (
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Control.CompilerError
import           Asm.Core.Data.Cpu
import           Asm.Core.Data.KindDefinition
import           Asm.Core.Data.MetaKey
import           Asm.Core.Data.TypeDefinition
import           Asm.Core.Phase1.Data.InitTree
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.SourcePos
import           Asm.Parser.Parser.Class
import           Asm.Parser.ToCompiler.Expr

import           Asm.Cpu6502.Compiler.Function
import           Asm.Cpu6502.Compiler.Phase2
import           Asm.Cpu6502.Compiler.Phase3
import           Asm.Cpu6502.Compiler.Phase4
import           Asm.Cpu6502.Compiler.Phase5
import           Asm.Cpu6502.Data.CpuData6502
import           Asm.Cpu6502.Data.FunctionKey
import           Asm.Cpu6502.Data.MetaKey
import           Asm.Cpu6502.Parser.CpuStmt

instance CpuParser Cpu6502 PStmtCpu6502 PExprCpu6502 where
  parseCpuStmt = parseCpu6502Stmt
  parseCpuExpr = mzero
  convertCpuExpr = [printInternalError|This cpu has no expressions|]
  convertCpuStmt PSRegular{..} = do
    s1rExpr <- mapM convertExpr s0rExpr
    let
      s1rOperator = s0rOperator
      s1rIndexMode = s0rIndexMode
      s1rAddressMode = s0rAddressMode
    return CS1Regular{..}
  convertCpuStmt PSInline{..} = do
    let
      s1iOperator = s0iOperator
      s1iIndexMode = s0iIndexMode
      s1iAddressMode = s0iAddressMode
      s1iName = s0iName
    return CS1Inline{..}
  convertCpuStmt PSData{..} = do
    s1dData <- mapM convertExpr s0dData
    return CS1Data{..}

instance Cpu Cpu6502 where
  -- phase 1
  cpuGetInitTree _ =
    [ I ("meta", KDNamespace,
        [ I ("cpu", KDMeta metaCpu, [])
        , I ("check", KDNamespace,
            [ I ("imm", KDMeta metaCheckImm, [])
            , I ("imm8", KDMeta metaCheckImm8, [])
            ]
          )
        ]
      )
    ]
  -- phase 2
  cpuLookupNamesExprC = [printInternalError|This cpu has no expressions|]
  cpuLookupNamesStmtC = cpu6502LookupNamesStmtC
  cpuFunctionKeys _ = cpu6502FunctionKeys
  -- phase 3
  cpuDefaultMetaData = mkmInsert metaOptimise (KDData TDBool, E4ConstBool spBuiltin True, spBuiltin) mkmEmpty
  cpuApplyMetaStmtC = cpu6502ApplyMetaStmtC
  cpuApplyMetaExprC = [printInternalError|This cpu has no expressions|]
  -- phase 4
  cpuConvertToStmt5C = cpu6502ConvertToStmt5C
  cpuCodeAlign _ = cpu6502CodeAlign
  cpuStmtMinMaxLength _ = cpu6502StmtMinMaxLength
  cpuStmtInlineNames _ CS4Inline{..} = [s4iName]
  cpuStmtInlineNames _ CS4Final{..}  = maybeToList s4fLabel
  cpuStmtInlineNames _ CS4Data{}     = []
  cpuMapExprInExpr = [printInternalError|This cpu has no expressions|]
  cpuMapExprInStmt f CS4Final{..}  = CS4Final{s4fData = map f s4fData, ..}
  cpuMapExprInStmt _ x@CS4Inline{} = x
  cpuMapExprInStmt f CS4Data{..}   = CS4Data{s4dData = map f s4dData, ..}
  -- phase 5
  cpuOptimiseLocalStmtBlockC = cpu6502OptimiseLocalStmtBlockC
  cpuOptimiseGlobalStmtBlockC = cpu6502OptimiseGlobalStmtBlockC
  cpuStmtBlockToByteValPiece = cpu6502StmtBlockToByteValPiece
  cpuStmt5MinMaxLength _ = cpu6502Stmt5MinMaxLength
  cpuStmt5InlineNames _ CS5LabelDefinition{..} = [s5lLabel]
  cpuStmt5InlineNames _ _                      = [] -- already converted to a label definition
  cpuLabelDefinitionStmt5C _ name = return [CS5LabelDefinition name]
  -- phases 34
  cpuSpecificFunctions = cpu6502SpecificFunctions
  cpuSpecificMetaFunctions = cpu6502SpecificMetaFunctions
