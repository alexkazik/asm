module Asm.Cpu6809.Compiler.Phase2 where

import           Asm.Core.Prelude

import           Asm.Core.Data.CpuData
import           Asm.Core.Phase2.CompilerState2
import           Asm.Core.Phase2.LookupNames
import           Asm.Core.Phase3.Data.Expr3
import           Asm.Core.SourcePos
import           Asm.Parser.Parser.LabelIdValue

import {-# SOURCE #-} Asm.Cpu6809.Data.Cpu6809       ()
import           Asm.Cpu6809.Data.CpuData6809
import           Asm.Cpu6809.Data.OpCodes

cpu6809LookupNamesStmtC :: Location -> CS12 Cpu6809 -> CSM2 Cpu6809 (Maybe (CS3 Cpu6809), [Expr3 Cpu6809])
cpu6809LookupNamesStmtC loc CS1Inline{..} = do
  s3iInline <- resolveNameC $sourcePos loc (labelIdValue loc s1iInline)
  return
    ( Just
        CS3Inline
          { s3iOperator = s1iOperator
          , s3iAM = s1iAM
          , s3iIW = s1iIW
          , ..
          }
    , []
    )

cpu6809LookupNamesStmtC _ CS1Indexed{..} = do
  s3xExpr <- mapM lookupNamesExprC s1xExpr
  return
    ( Just
        CS3Indexed
          { s3xOperator = s1xOperator
          , s3xIndexed = s1xIndexed
          , s3xIndirect = s1xIndirect
          , ..
          }
    , []
    )

cpu6809LookupNamesStmtC _ CS1Regular{..} = do
  s3rExpr <- mapM lookupNamesExprC s1rExpr
  let
    jumpTargets =
      if s1rOperator == oprJSR || s1rOperator == oprBSR
        then maybeToList s3rExpr
        else []
  return
    ( Just
        CS3Regular
          { s3rOperator = s1rOperator
          , s3rAM = s1rAM
          , ..
        }
    , jumpTargets
    )

cpu6809LookupNamesStmtC _ CS1Data{..} = do
  s3dExpr <- mapM lookupNamesExprC s1dExpr
  return (Just CS3Data{..}, [])
