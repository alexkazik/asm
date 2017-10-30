module Asm.Core.Phase2.LookupNames
  ( lookupNamesC
  , lookupNamesExprC
  ) where

import           Asm.Core.Prelude
import qualified Data.List                      as L

import           Asm.Core.Control.CompilerError
import           Asm.Core.Data.ByteVal
import           Asm.Core.Data.Cpu
import           Asm.Core.Data.Tree
import           Asm.Core.Phase1.Data.Expr12
import           Asm.Core.Phase2.CompilerState2
import           Asm.Core.Phase2.Data.Stmt2
import           Asm.Core.Phase3.Data.Expr3
import           Asm.Core.Phase3.Data.Stmt3
import           Asm.Core.SourcePos


lookupNamesC :: Cpu c => Stmt2Block c -> CSM2 c (Stmt3Block c)
lookupNamesC x = catMaybes <$> mapM (recoverFatalError Nothing . setNameDefinitionStmtC) x

{-
  set name definition: stmt
-}

setNameDefinitionStmtSubBlockC :: Cpu c => Text -> Stmt2Block c -> CSM2 c (Stmt3Block c)
setNameDefinitionStmtSubBlockC name block = do
  pushPathC name
  newBlock <- catMaybes <$> mapM setNameDefinitionStmtC block
  popPathC
  return newBlock

setNameDefinitionStmtC :: Cpu c => Stmt2 c -> CSM2 c (Maybe (Stmt3 c))

setNameDefinitionStmtC (S2Namespace loc name block) = do
  newBlock <- setNameDefinitionStmtSubBlockC name block
  return (Just $ S3Namespace loc name newBlock)

setNameDefinitionStmtC (S2Block loc name pool block) = do
  newBlock <- setNameDefinitionStmtSubBlockC name block
  pool' <- mapM lookupNamesExprC pool
  return (Just $ S3Block loc name pool' newBlock)

setNameDefinitionStmtC (S2For loc lname lvar from cmp to step block) = do
  newBlock <- setNameDefinitionStmtSubBlockC lname block
  from' <- lookupNamesExprC from
  to' <- lookupNamesExprC to
  step' <- lookupNamesExprC step
  return (Just $ S3For loc lname lvar from' cmp to' step' newBlock)

setNameDefinitionStmtC (S2IfBlock loc ifparam) = do
  ip' <- mapM setNameDefinitionIfC ifparam
  return (Just $ S3IfBlock loc ip')

setNameDefinitionStmtC (S2CpuStmt loc c) = do
  (sb, jt) <- cpuLookupNamesStmtC loc c
  cwd <- pathOfReference <$> currentPathC
  unless (null jt) $
    addCallPathsC cwd jt
  return (map (S3CpuStmt loc) sb)

setNameDefinitionStmtC (S2Variable loc vt name td v p al pg) = do
  v' <- mapM lookupNamesExprC v
  t <- lookupNamesExprC td
  p' <- mapM lookupNamesExprC p
  setPositionC name
  return (Just $ S3VariableUnresolved loc vt name t v' p' al pg)

setNameDefinitionStmtC (S2TypeDef _loc name td) = do
  t <- lookupNamesExprC td
  addTypeInExprC name t
  return Nothing

setNameDefinitionStmtC (S2LabelDefinition a b) = do
  setPositionC b
  return (Just $ S3LabelDefinition a b)
setNameDefinitionStmtC (S2MetaSet a b c) = do
  b' <- lookupNamesExprC b
  c' <- lookupNamesExprC c
  return (Just $ S3MetaSet a b' c')
setNameDefinitionStmtC (S2MetaUnset a b) = do
  b' <- lookupNamesExprC b
  return (Just $ S3MetaUnset a b')
setNameDefinitionStmtC (S2MetaSticky a b) = do
  b' <- lookupNamesExprC b
  return (Just $ S3MetaSticky a b')
setNameDefinitionStmtC (S2MetaUnsticky a b) = do
  b' <- lookupNamesExprC b
  return (Just $ S3MetaUnsticky a b')

setNameDefinitionBVExprC :: Cpu c => ByteVal (Expr12 c) -> CSM2 c (ByteVal (Expr3 c))
setNameDefinitionBVExprC = mapM lookupNamesExprC

setNameDefinitionIfC :: Cpu c => (Text, Expr12 c, Stmt2Block c) -> CSM2 c (Text, Expr3 c, Stmt3Block c)
setNameDefinitionIfC (name, e, block) = do
  e' <- lookupNamesExprC e
  newBlock <- setNameDefinitionStmtSubBlockC name block
  return (name, e', newBlock)


{-
  set name definition: Expr12
-}

lookupNamesExprC :: Cpu c => Expr12 c -> CSM2 c (Expr3 c)
lookupNamesExprC (E12LabelRef loc l) = do
  ni <- resolveNameC $sourcePos loc l
  getAliasC ni >>= \case
    Just alias ->
      lookupNamesExprC $ updateLocation ((locationOf alias L.\\ loc) ++ loc) alias
    Nothing ->
      return (E3LabelRef loc ni)

lookupNamesExprC (E12Pointer loc l t o) = do
  ni <- resolveNameC $sourcePos loc l
  return (E3Pointer loc ni t o)

lookupNamesExprC (E12DerefStruct loc e s) = do
  ne <- lookupNamesExprC e
  return (E3DerefStruct loc ne s)

lookupNamesExprC (E12DerefArray loc e a) = do
  ne <- lookupNamesExprC e
  na <- lookupNamesExprC a
  return (E3DerefArray loc ne na)

lookupNamesExprC (E12Function loc n es) = do
  es' <- mapM lookupNamesExprC es
  lookupFunctionName n >>= \case
    Just n' -> return (E3Function loc n' es')
    Nothing -> $throwFatalError [(loc, "Function with name \"" ++ show n ++ "\" not found")]

lookupNamesExprC (E12UserArray loc es f) = do
  es' <- mapM lookupNamesExprC es
  f' <- mapM lookupNamesExprC f
  return (E3UserArray loc es' f')

lookupNamesExprC (E12UserStructOrUnion loc es f) = do
  es' <- mapM lookupNamesExprC es
  f' <- mapM lookupNamesExprC f
  return (E3UserStructOrUnion loc es' f')

lookupNamesExprC (E12ConstInt loc a) = return (E3ConstInt loc a)
lookupNamesExprC (E12ConstMaskedInt loc a) = return (E3ConstMaskedInt loc a)
lookupNamesExprC (E12ConstBool loc a) = return (E3ConstBool loc a)
lookupNamesExprC (E12ByteVal loc a) = do
  a' <- setNameDefinitionBVExprC a
  return (E3ByteVal loc a')
lookupNamesExprC (E12CpuExpr loc a) = do
  a' <- cpuLookupNamesExprC a
  return (E3CpuExpr loc a')
lookupNamesExprC (E12UserArrayBVS loc a b) = do
  b' <- mapM lookupNamesExprC b
  return (E3UserArrayBVS loc a b')
lookupNamesExprC (E12DefineArray loc a b) = do
  a' <- lookupNamesExprC a
  b' <- mapM lookupNamesExprC b
  return (E3DefineArray loc a' b')
lookupNamesExprC (E12TypeStruct loc a) = do
  a' <- mapM (\(t,b) -> (,) t <$> lookupNamesExprC b) a
  return (E3TypeStruct loc a')
lookupNamesExprC (E12TypeUnion loc a) = do
  a' <- mapM (\(t,b) -> (,) t <$> lookupNamesExprC b) a
  return (E3TypeUnion loc a')
lookupNamesExprC (E12MagicValue loc a) = return (E3MagicValue loc a)
