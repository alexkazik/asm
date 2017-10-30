module Asm.Core.Phase2.RegisterTypes
  ( registerTypesC
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Control.CompilerError
import           Asm.Core.Data.Cpu
import           Asm.Core.Data.KindDefinition
import           Asm.Core.Data.TypeDefinition
import           Asm.Core.Phase2.CompilerState2
import           Asm.Core.Phase2.LookupNames
import           Asm.Core.Phase3.Data.Expr3
import           Asm.Core.Phase3.Data.Stmt3
import           Asm.Core.SourcePos


registerTypesC :: Cpu c => Stmt3Block c -> CSM2 c (Stmt3Block c)
registerTypesC = mapM registerTypesStmtC

registerTypesStmtC :: Cpu c => Stmt3 c -> CSM2 c (Stmt3 c)


-- remove the namespace layer
registerTypesStmtC (S3Namespace loc n block) = do
  block' <- registerTypesC block
  return (S3Namespace loc n block')

-- block: add to pool
registerTypesStmtC (S3Block loc n pool block) = do
  block' <- registerTypesC block
  return (S3Block loc n pool block')

registerTypesStmtC (S3For loc n var from cmp to step block) = do
  block' <- registerTypesC block
  return (S3For loc n var from cmp to step block')

registerTypesStmtC (S3IfBlock loc blocks) = do
  blocks' <- mapM registerTypesIfC blocks
  return (S3IfBlock loc blocks')

registerTypesStmtC (S3VariableUnresolved loc vt n t v p al pg) = do
  ty <- getTypeDefinition [(loc, "stmt")] t
  setKindC n (loc, KDPointer ty)
  return (S3Variable loc vt n ty v p al pg)

registerTypesStmtC x@S3Variable{} = return x
registerTypesStmtC x@S3LabelDefinition{} = return x
registerTypesStmtC x@S3CpuStmt{} = return x
registerTypesStmtC x@S3MetaSet{} = return x
registerTypesStmtC x@S3MetaUnset{} = return x
registerTypesStmtC x@S3MetaSticky{} = return x
registerTypesStmtC x@S3MetaUnsticky{} = return x


registerTypesIfC :: Cpu c => (Text, Expr3 c, Stmt3Block c) -> CSM2 c (Text, Expr3 c, Stmt3Block c)
registerTypesIfC (t, e, blocks) = do
  blocks' <- registerTypesC blocks
  return (t, e, blocks')


getTypeDefinition :: Cpu c => [(Location,String)] -> Expr3 c -> CSM2 c TypeDefinition

getTypeDefinition err (E3LabelRef loc l') =
  -- all ELabelRef's are checked for existence
  getKindC l' >>= \case
    (_, KDType ty) -> return ty
    (_, KDTypeInExpr) -> getTypeInExprC l' >>= getTypeDefinition ((loc, "expr"):err)
    (_, KDAlias) -> getAliasC l' >>= $fromJustOrError [(loc, "alias not found")] >>= lookupNamesExprC >>= getTypeDefinition ((loc, "expr"):err)
    _ -> $throwFatalError ((loc, "can't read the type"):err)

getTypeDefinition err (E3DefineArray loc a e) = do
  a' <- getTypeDefinition ((loc,"expr"):err) a
  case e of
    Just (E3ConstInt _ i) ->
      if i >= 0
        then return (TDArray a' (Just i))
        else $throwFatalError ((loc, "can't read the type"):err)
    Nothing ->
      return (TDArray a' Nothing)
    _ -> $throwFatalError ((loc, "can't read the type"):err)

getTypeDefinition err (E3TypeStruct loc str) = do
  str' <- mapM evalStr str
  return (TDStruct str')
  where
    evalStr (n,t) = do
      let na = n
      x <- getTypeDefinition ((loc, "expr"):err) t
      return (na, x)

getTypeDefinition err (E3TypeUnion loc str) = do
  str' <- mapM evalStr str
  return (TDUnion str')
  where
    evalStr (n,t) = do
      let na = n
      x <- getTypeDefinition ((loc, "expr"):err) t
      return (na, x)

getTypeDefinition err expr = $throwFatalError ((locationOf expr, "can't read the type"):err)
