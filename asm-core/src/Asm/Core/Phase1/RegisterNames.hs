module Asm.Core.Phase1.RegisterNames
  ( registerNamesC
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Data.Cpu
import           Asm.Core.Data.KindDefinition
import           Asm.Core.Data.TypeDefinition
import           Asm.Core.Data.VariableType
import           Asm.Core.Phase1.CompilerState1
import           Asm.Core.Phase1.Data.Expr12
import           Asm.Core.Phase1.Data.Stmt1
import           Asm.Core.Phase2.Data.Stmt2
import           Asm.Core.SourcePos

{-
  get name definition: Stmt
-}

registerNamesC :: Cpu c => Stmt1Block c -> CSM1 c (Stmt2Block c)
registerNamesC x = catMaybes <$> mapM registerNamesStmtC x


registerNamesSubBlockC :: Cpu c => Text -> Stmt1Block c -> CSM1 c (Stmt2Block c)
registerNamesSubBlockC name block = do
  pushPathC name
  newBlock <- catMaybes <$> mapM registerNamesStmtC block
  popPathC
  return newBlock


registerNamesStmtC :: Cpu c => Stmt1 c -> CSM1 c (Maybe (Stmt2 c))

registerNamesStmtC (S1LabelDefinition loc name) = (Just . S2LabelDefinition loc) <$> addNameC name (loc, KDPointer TDCode)

registerNamesStmtC (S1Variable loc vt name td v p al pg) = do
  when (not (isPrefixOf "_" name) && vt == VTLocal) $ printErrorC ((loc, "local variable must start with a underscore"):[sourcePos||])
  n <- addNameC name (loc, KDPointer TDInvalid)
  return $ Just $ S2Variable loc vt n td v p al pg

registerNamesStmtC (S1Namespace loc n block) = do
  name <- maybe getUniqueNameC return n
  _ <- addNameC name (loc, KDNamespace)
  newBlock <- registerNamesSubBlockC name block
  return (Just $ S2Namespace loc name newBlock)

registerNamesStmtC (S1Block loc n pool block) = do
  name <- maybe getUniqueNameC return n
  _ <- addNameC name (loc, KDNamespace)
  newBlock <- registerNamesSubBlockC name block
  return (Just $ S2Block loc name pool newBlock)

registerNamesStmtC (S1For loc n from cmp to step block) = do
  n' <- addNameC n (loc, KDLoopVariable)
  name <- getUniqueNameC
  _ <- addNameC name (loc, KDNamespace)
  pushSystemNameModeC
  newBlock <- registerNamesSubBlockC name block
  popSystemNameModeC
  return (Just $ S2For loc name n' from cmp to step newBlock)

registerNamesStmtC (S1IfBlock loc ifparam) = (Just . S2IfBlock loc) <$> mapM (registerNamesIfC loc) ifparam

registerNamesStmtC (S1PoolStmt l n [] v s b) = do
  addPoolBothC l n v s b
  return Nothing

registerNamesStmtC (S1PoolStmt l n u v s b) = do
  addPoolC l n u v s b
  return Nothing

registerNamesStmtC (S1TypeDef loc name td) = do
   n <- addNameC name (loc, KDTypeInExpr)
   return (Just $ S2TypeDef loc n td)

registerNamesStmtC (S1Alias loc name value) = do
   n <- addNameC name (loc, KDAlias)
   addAliasC n value
   return Nothing

registerNamesStmtC (S1CpuStmt a b) = return (Just $ S2CpuStmt a b)
registerNamesStmtC (S1MetaSet a b c) = return (Just $ S2MetaSet a b c)
registerNamesStmtC (S1MetaUnset a b) = return (Just $ S2MetaUnset a b)
registerNamesStmtC (S1MetaSticky a b) = return (Just $ S2MetaSticky a b)
registerNamesStmtC (S1MetaUnsticky a b) = return (Just $ S2MetaUnsticky a b)

registerNamesIfC :: Cpu c => Location -> (Text, Expr12 c, Stmt1Block c) -> CSM1 c (Text, Expr12 c, Stmt2Block c)
registerNamesIfC loc (_n, e, block) = do
  name <- getUniqueNameC
  _ <- addNameC name (loc, KDNamespace)
  pushSuperLocalsModeC
  newBlock <- registerNamesSubBlockC name block
  popSuperLocalsModeC
  return (name, e, newBlock)
