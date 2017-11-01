{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Asm.Cpu6809.Data.Cpu6809
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

import           Asm.Cpu6809.Compiler.Function
import           Asm.Cpu6809.Compiler.Phase2
import           Asm.Cpu6809.Compiler.Phase3
import           Asm.Cpu6809.Compiler.Phase4
import           Asm.Cpu6809.Compiler.Phase5
import           Asm.Cpu6809.Data.CpuData6809
import           Asm.Cpu6809.Data.FunctionKey
import           Asm.Cpu6809.Data.MetaKey
import           Asm.Cpu6809.Parser.CpuStmt

instance CpuParser Cpu6809 PStmtCpu6809 PExprCpu6809 where
  parseCpuStmt = parseCpu6809Stmt
  parseCpuExpr = mzero
  convertCpuExpr = [printInternalError|This cpu has no expressions|]
  convertCpuStmt PSInline{..} =
    return
      CS1Inline
        { s1iOperator = s0iOperator
        , s1iAM = s0iAM
        , s1iIW = s0iIW
        , s1iInline = s0iInline
        }
  convertCpuStmt PSIndexed{..} = do
    s1xExpr <- mapM convertExpr s0xExpr
    return
      CS1Indexed
        { s1xOperator = s0xOperator
        , s1xIndexed = s0xIndexed
        , s1xIndirect = s0xIndirect
        , ..
        }
  convertCpuStmt PSRegular{..} = do
    s1rExpr <- mapM convertExpr s0rExpr
    return
      CS1Regular
        { s1rOperator = s0rOperator
        , s1rAM = s0rAM
        , ..
        }
  convertCpuStmt PSData{..} = do
    s1dExpr <- mapM convertExpr s0dExpr
    return CS1Data{..}

instance Cpu Cpu6809 where
  -- phase 1
  cpuGetInitTree _ =
    [ I ("meta", KDNamespace,
        [ I ("cpu", KDMeta metaCpu, [])
        , I ("check", KDNamespace,
            [ I ("imm", KDMeta metaCheckImm, [])
            , I ("imm8", KDMeta metaCheckImm8, [])
            ]
          )
        , I ("dp", KDMeta metaDP, [])
        , I ("x", KDMeta metaX, [])
        , I ("y", KDMeta metaY, [])
        , I ("u", KDMeta metaU, [])
        , I ("s", KDMeta metaS, [])
        , I ("optimise", KDMeta metaOptimise, [])
        ]
      )
    ]
  -- phase 2
  cpuLookupNamesExprC = [printInternalError|This cpu has no expressions|]
  cpuLookupNamesStmtC = cpu6809LookupNamesStmtC
  cpuFunctionKeys _ = cpu6809FunctionKeys
  -- phase 3
  cpuDefaultMetaData = mkmInsert metaOptimise (KDData TDBool, E4ConstBool spBuiltin True, spBuiltin) mkmEmpty
  cpuApplyMetaStmtC = cpu6809ApplyMetaStmtC
  cpuApplyMetaExprC = [printInternalError|This cpu has no expressions|]
  -- phase 4
  cpuConvertToStmt5C = cpu6809ConvertToStmt5C
  cpuCodeAlign _ = cpu6809CodeAlign
  cpuStmtMinMaxLength _ = cpu6809StmtMinMaxLength
  cpuStmtInlineNames _ CS4Inline{..} = [s4iInline]
  cpuStmtInlineNames _ CS4Final{..}  = maybeToList s4fInline
  cpuStmtInlineNames _ CS4Data{}     = []
  cpuMapExprInExpr = [printInternalError|This cpu has no expressions|]
  cpuMapExprInStmt f CS4Final{..}  = CS4Final{s4fData = map f s4fData, ..}
  cpuMapExprInStmt _ x@CS4Inline{} = x
  cpuMapExprInStmt f CS4Data{..}   = CS4Data{s4dExpr = map f s4dExpr, ..}
  -- phase 5
  cpuOptimiseLocalStmtBlockC = cpu6809OptimiseLocalStmtBlockC
  cpuOptimiseGlobalStmtBlockC = cpu6809OptimiseGlobalStmtBlockC
  cpuStmtBlockToByteValPiece = cpu6809StmtBlockToByteValPiece
  cpuStmt5MinMaxLength _ = cpu6809Stmt5MinMaxLength
  cpuStmt5InlineNames _ CS5LabelDefinition{..} = [s5lLabel]
  cpuStmt5InlineNames _ _                      = [] -- already converted to a label definition
  cpuLabelDefinitionStmt5C _ name = return [CS5LabelDefinition name]
  -- phases 34
  cpuSpecificFunctions = cpu6809SpecificFunctions
  cpuSpecificMetaFunctions = cpu6809SpecificMetaFunctions
