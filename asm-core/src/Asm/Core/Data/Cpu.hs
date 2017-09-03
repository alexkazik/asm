{-# LANGUAGE TypeFamilies #-}

module Asm.Core.Data.Cpu
  ( Cpu(..)
  , CpuSource(..)
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Data.ByteValPiece
import           Asm.Core.Data.CpuData
import           Asm.Core.Data.FunctionKey
import           Asm.Core.Data.KindDefinition
import           Asm.Core.Data.MetaKey
import           Asm.Core.Data.Reference
import           Asm.Core.Phase1.Data.InitTree
import           Asm.Core.Phase1.Data.Stmt1
import           Asm.Core.Phase2.Data.CompilerState2
import           Asm.Core.Phase3.Data.CompilerState3
import           Asm.Core.Phase3.Data.Expr3
import           Asm.Core.Phase4.Data.CompilerState4
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.Phase4.Data.Stmt4
import           Asm.Core.Phases34.Data.CompilerState34
import           Asm.Core.Phases34.Data.Function
import           Asm.Core.SourcePos

class CpuData c => Cpu c where
  -- phase 1
  cpuGetInitTree :: proxy c -> [InitTree]
  -- phase 2
  cpuLookupNamesExprC :: CE12 c -> CSM2 c (CE3 c)
  cpuLookupNamesStmtC :: Location -> CS12 c -> CSM2 c (Maybe (CS3 c), [Expr3 c])
  cpuFunctionKeys :: proxy c -> [FunctionKey]
  -- phase 3
  cpuDefaultMetaData :: MetaKeyMap (KindDefinition, Expr4 c, Location)
  cpuApplyMetaExprC :: Location -> CE3 c -> CSM3 c (CE4 c)
  cpuApplyMetaStmtC :: Location -> CS3 c -> CSM3 c (Stmt4Block c)
  -- phase 4
  cpuConvertToStmt5C :: Location -> CS4 c -> CSM4 c (CS5Block c)
  cpuLabelDefinitionStmt5C :: Location -> Reference -> CSM4 c (CS5Block c)
  cpuCodeAlign :: proxy c -> Int64
  cpuStmtMinMaxLength :: proxy c -> CS4 c -> (Int64, Int64)
  cpuStmtInlineNames :: proxy c -> CS4 c -> [Reference]
  cpuMapExprInExpr :: (Expr4 c -> Expr4 c) -> CE4 c -> CE4 c
  cpuMapExprInStmt :: (Expr4 c -> Expr4 c) -> CS4 c -> CS4 c
  -- phase 5
  cpuOptimiseLocalStmtBlockC :: CS5Block c -> CSM4 c (CS5Block c)
  cpuOptimiseGlobalStmtBlockC :: [CS5Block c] -> CSM4 c [CS5Block c]
  cpuStmtBlockToByteValPiece :: CS5Block c -> ByteValPiece (Expr4 c)
  cpuStmt5MinMaxLength :: proxy c -> CS5 c -> (Int64, Int64)
  cpuStmt5InlineNames :: proxy c -> CS5 c -> [Reference]
  -- phases 34
  cpuSpecificFunctions :: CSM34 m => [(FunctionKey, Function m c)]
  cpuSpecificMetaFunctions :: [(FunctionKey, (FunctionKey -> Location -> [Expr4 c] -> CSM3 c (Expr4 c)))]

class Cpu c => CpuSource c where
  type Source c
  fromSource :: Source c -> Stmt1Block c
