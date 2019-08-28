{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Asm.Parser.ToCompiler.Stmt
  ( genericFromSource
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Control.CompilerError
import           Asm.Core.Phase1.Data.Expr12
import           Asm.Core.Phase1.Data.Stmt1

import           Asm.Parser.Data.PExpr
import           Asm.Parser.Data.PStmt
import           Asm.Parser.Parser.Tools
import           Asm.Parser.ToCompiler.Expr
import           Asm.Parser.ToCompiler.PCState


genericFromSource :: CpuParser c ps pe => [PStmt ps pe] -> Stmt1Block c
genericFromSource block = evalState (convertStmtBlock block) PCSD {pcsPath = []}

convertStmtBlock :: CpuParser c ps pe => [PStmt ps pe] -> PCSM (Stmt1Block c)
convertStmtBlock = mapM convertStmt

convertStmt :: CpuParser c ps pe => PStmt ps pe -> PCSM (Stmt1 c)
convertStmt (loc, PSNamespace name block) = do
  loc' <- getPathPC loc
  block' <- convertStmtBlock block
  let name' = map (labelIdValue loc') name
  return $ S1Namespace loc' name' block'
convertStmt (loc, PSBlock name pool block) = do
  loc' <- getPathPC loc
  block' <- convertStmtBlock block
  let name' = map (labelIdValue loc') name
  pool' <- mapM convertExpr pool
  return $ S1Block loc' name' pool' block'
convertStmt (loc, PSFor name from cmp to step block) = do
  loc' <- getPathPC loc
  block' <- convertStmtBlock block
  let name' = labelIdValue loc' name
  from' <- convertExpr from
  to' <- convertExpr to
  step' <- convertExpr step
  return $ S1For loc' name' from' (bool ForCmpLessThan ForCmpLessEqual cmp) to' step' block'
convertStmt (loc, PSIfBlock blocks) = do
  loc' <- getPathPC loc
  blocks' <- mapM convertIfBlock blocks
  return $ S1IfBlock loc' blocks'
convertStmt (loc, PSLabelDefinition l) = do
  loc' <- getPathPC loc
  let l' = labelIdValue loc' l
  return $ S1LabelDefinition loc' l'
convertStmt (loc, PSCpuStmt c) = do
  loc' <- getPathPC loc
  c' <- convertCpuStmt c
  return $ S1CpuStmt loc' c'
convertStmt (loc, PSMetaSet k v) = do
  loc' <- getPathPC loc
  k' <- convertExpr k
  v' <- convertExpr v
  return $ S1MetaSet loc' k' v'
convertStmt (loc, PSMetaUnset k) = do
  loc' <- getPathPC loc
  k' <- convertExpr k
  return $ S1MetaUnset loc' k'
convertStmt (loc, PSMetaSticky k) = do
  loc' <- getPathPC loc
  k' <- convertExpr k
  return $ S1MetaSticky loc' k'
convertStmt (loc, PSMetaUnsticky k) = do
  loc' <- getPathPC loc
  k' <- convertExpr k
  return $ S1MetaUnsticky loc' k'
convertStmt (loc, PSVariable vt n t v p al pg) = do
  loc' <- getPathPC loc
  t' <- convertExpr t
  v' <- mapM convertExpr v
  let al' = int64Value loc' al
  let pg' = map (int64Value loc') pg
  let n' = labelIdValue loc' n
  p' <- mapM convertExpr p
  return $ S1Variable loc' vt n' t' v' p' al' pg'
convertStmt (loc, PSPoolStmt n u v s b) = do
  loc' <- getPathPC loc
  let n' = labelIdValue loc' n
  let u' = map (labelIdValue loc') u
  return $ S1PoolStmt loc' n' u' v (int64Value loc' s) (int64Value loc' b)
convertStmt (loc, PSTypeDef n t) = do
  loc' <- getPathPC loc
  t' <- convertExpr t
  let n' = labelIdValue loc' n
  return $ S1TypeDef loc' n' t'
convertStmt (loc, PSTraceStep e) = do
  pushPathPC loc
  e' <- convertStmt e
  popPathPC
  return e'
convertStmt (loc, PSAlias n t) = do
  loc' <- getPathPC loc
  t' <- convertExpr t
  let n' = labelIdValue loc' n
  return $ S1Alias loc' n' t'

-- all the following statements should have been either replaced, removed or thrown an error earlier
convertStmt (loc, x@PSBuildIf{}) = $printError [([loc], "convertStmt: " ++ show x)]
convertStmt (loc, x@PSBuildElseif{}) = $printError [([loc], "convertStmt: " ++ show x)]
convertStmt (loc, x@PSBuildElse) = $printError [([loc], "convertStmt: " ++ show x)]
convertStmt (loc, x@PSBuildEndif) = $printError [([loc], "convertStmt: " ++ show x)]
convertStmt (loc, x@PSBuildNamespace{}) = $printError [([loc], "convertStmt: " ++ show x)]
convertStmt (loc, x@PSBuildBlock{}) = $printError [([loc], "convertStmt: " ++ show x)]
convertStmt (loc, x@PSBuildFor{}) = $printError [([loc], "convertStmt: " ++ show x)]
convertStmt (loc, x@PSBuildEnd) = $printError [([loc], "convertStmt: " ++ show x)]
convertStmt (loc, x@PSBuildDirectIf{}) = $printError [([loc], "convertStmt: " ++ show x)]
convertStmt (loc, x@PSBuildDirectElseif{}) = $printError [([loc], "convertStmt: " ++ show x)]
convertStmt (loc, x@PSBuildDirectElse) = $printError [([loc], "convertStmt: " ++ show x)]
convertStmt (loc, x@PSBuildDirectEndif) = $printError [([loc], "convertStmt: " ++ show x)]
convertStmt (loc, x@PSAntiBuildDirectIfBlock{}) = $printError [([loc], "convertStmt: " ++ show x)]
convertStmt (loc, x@PSAntiNamespace{}) = $printError [([loc], "convertStmt: " ++ show x)]
convertStmt (loc, x@PSAntiBlock{}) = $printError [([loc], "convertStmt: " ++ show x)]
convertStmt (loc, x@PSNothing) = $printError [([loc], "convertStmt: " ++ show x)]

convertIfBlock :: CpuParser c ps pe => (PExpr pe, [PStmt ps pe]) -> PCSM (Text, Expr12 c, Stmt1Block c)
convertIfBlock (e, block) = do
  e' <- convertExpr e
  block' <- convertStmtBlock block
  return ("", e', block')
