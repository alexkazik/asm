{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Asm.Cpu6502.Compiler.Phase2
  ( cpu6502LookupNamesStmtC
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Data.CpuData
import           Asm.Core.Phase2.CompilerState2
import           Asm.Core.Phase2.LookupNames
import           Asm.Core.Phase3.Data.Expr3
import           Asm.Core.SourcePos
import           Asm.Parser.Parser.LabelIdValue

import {-# SOURCE #-} Asm.Cpu6502.Data.Cpu6502       ()
import           Asm.Cpu6502.Data.CpuData6502
import           Asm.Cpu6502.Data.OpCodes


--
-- convert phase 1/2 to 3: replace names by reference
--

cpu6502LookupNamesStmtC :: Location -> CS12 Cpu6502 -> CSM2 Cpu6502 (Maybe (CS3 Cpu6502), [Expr3 Cpu6502])

cpu6502LookupNamesStmtC _ CS1Regular{..} = do
  s3rExpr <- mapM lookupNamesExprC s1rExpr
  let
    s3rOperator = s1rOperator
    s3rIndexMode = s1rIndexMode
    s3rAddressMode = s1rAddressMode
  return
    ( Just CS3Regular{..}
    , if s1rOperator == oprJSR
        then maybeToList s3rExpr
        else []
    )

cpu6502LookupNamesStmtC loc CS1Inline{..} = do
  s3iName <- resolveNameC $sourcePos loc (labelIdValue loc s1iName)
  let
     s3iOperator = s1iOperator
     s3iIndexMode = s1iIndexMode
     s3iAddressMode = s1iAddressMode
  return (Just CS3Inline{..}, [])

cpu6502LookupNamesStmtC _ CS1Data{..} = do
  s3dData <- mapM lookupNamesExprC s1dData
  return (Just CS3Data{..}, [])
