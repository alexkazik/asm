module Asm.Core.Phase3.MetaDataApply
  ( module Asm.Core.Phase3.MetaDataApply
  , module Asm.Core.Phase3.MetaData
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Data.Cpu
import           Asm.Core.Data.FunctionKey
import           Asm.Core.Data.KindDefinition
import           Asm.Core.Phase3.CompilerState3
import           Asm.Core.Phase3.Data.Expr3
import           Asm.Core.Phase3.MetaData
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.Phases34.Function
import           Asm.Core.PrettyPrint.Use
import           Asm.Core.SourcePos

{-
  During the PlaceInPool task the meta-data is collected and whenever
  something is placed the applyMeta functions are called.
  This is the only time when getMetaC will return the correct result.
-}

{-
    applyMetaExprC
-}

applyMetaExprC :: Cpu c => Expr3 c -> CSM3 c (Expr4 c)
applyMetaExprC (E3UserArray loc es f) = do
  es' <- mapM applyMetaExprC es
  f' <- mapM applyMetaExprC f
  return (E4UserArray loc es' f')
applyMetaExprC (E3UserStructOrUnion loc es f) = do
  es' <- mapM applyMetaExprC es
  f' <- mapM applyMetaExprC f
  return (E4UserStructOrUnion loc es' f')
applyMetaExprC (E3DerefArray loc a e) = do
  a' <- applyMetaExprC a
  e' <- applyMetaExprC e
  return (E4DerefArray loc a' e')
applyMetaExprC (E3DerefStruct loc s e) = do
  s' <- applyMetaExprC s
  case s' of
    (E4NamespaceRef l1 n) -> do
      l' <- resolveNameC [sourcePos||] loc e n
      applyMetaExprC (E3LabelRef l1 l')
    _ -> return (E4DerefStruct loc s' e)
applyMetaExprC (E3Function loc f es) = do
  es' <- mapM applyMetaExprC es
  case fkmLookup f metaFunctions of
      Just fn -> fn f loc es'
      Nothing -> return (E4Function loc f es')
applyMetaExprC (E3CpuExpr loc c) = do
  c' <- cpuApplyMetaExprC loc c
  return (E4CpuExpr loc c')

applyMetaExprC (E3ConstInt loc a) = return (E4ConstInt loc a)
applyMetaExprC (E3ConstMaskedInt loc a) = return (E4ConstMaskedInt loc a)
applyMetaExprC (E3ConstBool loc a) = return (E4ConstBool loc a)
applyMetaExprC (E3ByteVal loc a) = do
  a' <- mapM applyMetaExprC a
  return (E4ByteVal loc a')
applyMetaExprC (E3Pointer loc a b c) = return (E4Pointer loc a b c)
applyMetaExprC (E3UserArrayBVS loc a b) = do
  b' <- mapM applyMetaExprC b
  return (E4UserArrayBVS loc a b')
applyMetaExprC (E3DefineArray loc a b) = do
  a' <- applyMetaExprC a
  b' <- mapM applyMetaExprC b
  return (E4DefineArray loc a' b')
applyMetaExprC (E3TypeStruct loc a) = do
  a' <- mapM applyMetaExprSndC a
  return (E4TypeStruct loc a')
applyMetaExprC (E3TypeUnion loc a) = do
  a' <- mapM applyMetaExprSndC a
  return (E4TypeUnion loc a')
applyMetaExprC (E3LabelRef loc l) =
  getKindC l >>= \case
    (_, KDPointer ty) -> return (E4Pointer loc l ty 0)
    (_, KDNamespace) -> return (E4NamespaceRef loc l)
    (_, KDType ty) -> return (E4Type loc ty 0)
    (_, KDTypeInExpr) -> do
        t <- getTypeInExprC l
        applyMetaExprC t
    (_, KDMeta m) -> return (E4Meta loc m)
    (_, KDLoopVariable) -> return (E4LoopVariable loc l)
    (loc', x) -> printErrorC $ (loc, "type of labelref unknown: " ++ showPretty x):(loc', "definition"):[sourcePos||]
applyMetaExprC (E3MagicValue loc a) = return (E4MagicValue loc a)

applyMetaExprSndC :: Cpu c => (t, Expr3 c) -> CSM3 c (t, Expr4 c)
applyMetaExprSndC (a, b) = do
  b' <- applyMetaExprC b
  return (a, b')
