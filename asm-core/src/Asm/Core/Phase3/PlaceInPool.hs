{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Asm.Core.Phase3.PlaceInPool
  ( placeInPoolC
  ) where

import           Asm.Core.Prelude
import qualified Data.Map.Strict                  as M
import qualified Data.Vector                      as V

import           Asm.Core.Control.CompilerError
import           Asm.Core.Data.ByteVal
import           Asm.Core.Data.ByteValPiece
import           Asm.Core.Data.Cpu
import           Asm.Core.Data.FunctionKey
import           Asm.Core.Data.KindDefinition
import           Asm.Core.Data.MetaKey
import           Asm.Core.Data.TypeDefinition
import           Asm.Core.Data.VariableType
import           Asm.Core.Flags
import           Asm.Core.Phase3.CompilerState3
import           Asm.Core.Phase3.Data.Expr3
import           Asm.Core.Phase3.Data.Stmt3
import           Asm.Core.Phase3.MetaDataApply
import           Asm.Core.Phase3.Pool
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.Phase4.Data.Stmt4
import           Asm.Core.Phases34.EvaluateExpr
import           Asm.Core.Phases34.Function.Check
import           Asm.Core.Phases34.TypeDefinition
import           Asm.Core.PrettyPrint.Use
import           Asm.Core.SourcePos


placeInPoolC :: Cpu c => Stmt3Block c -> CSM3 c (Stmt4Block c)
placeInPoolC block = concat <$> mapM (recoverFatalError [] . placeStmtInPoolC) block

-- all following functions throws an fatal error, which is recovered above

wrapMetaLevel :: Cpu c => CSM3 c a -> CSM3 c a
wrapMetaLevel x = do
  meta <- getAllMetaC
  sticky <- getAllMetaStickyC
  x' <- x
  newMeta <- getAllMetaC
  newSticky <- getAllMetaStickyC
  let
    isSticky = mksUnion sticky newSticky
    meta' = foldl' useSticky meta (mksToList isSticky)
      where
        useSticky mt k =
          case mkmLookup k newMeta of
            Just v  -> mkmInsert k v mt
            Nothing -> mkmDelete k mt
  setAllMetaC meta'
  setAllMetaStickyC sticky
  return x'


placeStmtInPoolC :: Cpu c => Stmt3 c -> CSM3 c (Stmt4Block c)

-- remember meta
placeStmtInPoolC (S3MetaSet loc k v) = do
  k' <- evaluateExprTopC =<< applyMetaExprC k
  v' <- evaluateExprTopC =<< applyMetaExprC v
  case k' of
    (KDMeta k'', _) -> do
      setMetaExprC k'' v' loc
      return []
    _ -> $throwFatalError [(loc, "set meta with non meta key")]
placeStmtInPoolC (S3MetaUnset loc k) = do
  k' <- evaluateExprTopC =<< applyMetaExprC k
  case k' of
    (KDMeta k'', _) -> do
      unsetMetaExprC k''
      return []
    _ -> $throwFatalError [(loc, "unset meta with non meta key")]
placeStmtInPoolC (S3MetaSticky loc k) = do
  k' <- evaluateExprTopC =<< applyMetaExprC k
  case k' of
    (KDMeta k'', _) -> do
      stickyMetaExprC k''
      return []
    _ -> $throwFatalError [(loc, "sticky meta with non meta key")]
placeStmtInPoolC (S3MetaUnsticky loc k) = do
  k' <- evaluateExprTopC =<< applyMetaExprC k
  case k' of
    (KDMeta k'', _) -> do
      unstickyMetaExprC k''
      return []
    _ -> $throwFatalError [(loc, "unsticky meta with non meta key")]

-- remove the namespace layer
placeStmtInPoolC (S3Namespace _ _ block) =
  wrapMetaLevel $ placeInPoolC block

-- block: add to pool
placeStmtInPoolC (S3Block loc _ poolMay block) =
  wrapMetaLevel $ do
    forM_ poolMay $ \pool -> do
      pool' <- evaluateExprTopC =<< applyMetaExprC pool
      setMetaExprC metaPoolCode pool' loc
    pl <- $fromJustOrError [(loc, "meta.pool.code is not set")] =<< getMetaExprMayC [metaPoolCode]
    p' <- getPoolElemRefC loc pl
    block' <- placeInPoolC block

    unless (null block') $
      addCodeToPoolC (locationOf $ headEx block') p' block'

    return []

-- loop: keep all important stuff
placeStmtInPoolC (S3For loc _ var from cmp to step block) =
  wrapMetaLevel $ do
    from' <- applyMetaExprC from
    to' <- applyMetaExprC to
    step' <- applyMetaExprC step
    block' <- placeInPoolC block
    return [S4For loc var from' cmp to' step' block']

placeStmtInPoolC (S3IfBlock loc blocks) = do
  nb <- mapM placeIfC blocks
  return [S4IfBlock loc nb]

placeStmtInPoolC (S3LabelDefinition loc n) = do
  pl <- getMetaExprMayC [metaPoolCode]
  pl' <- mapM (getPoolElemRefC loc) pl
  setPositionC n (map fst pl', Left (minBound, maxBound))
  return [S4LabelDefinition loc n]

placeStmtInPoolC stmt@S3VariableUnresolved{} = $throwFatalError [(locationOf stmt, "S3Variable:Unresolved")]
placeStmtInPoolC (S3Variable loc VTPointer n ty v p al pg) = do
  when (al /= 1 || isJust pg) $ $throwFatalError [(loc, "only a var or const can use align/page")]
  when (ty == poolType) $ $throwFatalError [(loc, "a pool can only be used in a var or const")]
  when (isJust p) $ $throwFatalError [(loc, "pointer can't have a pool")]
  case v of
    Just vv ->
      applyMetaExprC vv >>= evaluateExprTopC >>= \case
        (_, E4ConstInt _ i) ->
          setPositionC n (Nothing, Right i)
        _ -> $throwFatalError [(loc, "pointer variable must be an integer and immediately solvable")]
    Nothing -> $throwFatalError [(loc, "pointer variable must have an initial value")]
  return []
placeStmtInPoolC (S3Variable loc vt n ty v p al pg) = do
  case v of
    Just _ ->
      when (vt == VTLocal) $
        $throwFatalError [(loc, "local variable must not have an initial value")]
    Nothing ->
      when (vt == VTConst) $
        $throwFatalError [(loc, "const variable must have an initial value")]
  unless (vt == VTVar || vt == VTConst) $ do
    when (al /= 1 || isJust pg) $
      $throwFatalError [(loc, "only a var or const can use align/page")]
    when (ty == poolType) $
      $throwFatalError [(loc, "a pool can only be used in a var or const")]
  q <-
    case p of
      Just p' -> do
        when (vt == VTInline) $ $throwFatalError [(loc, "inline can't have a pool")]
        (poolKind, poolValue) <- evaluateExprTopC =<< applyMetaExprC p'
        return (poolKind, poolValue, loc)
      Nothing
        | ty == poolType -> $throwFatalError [(loc, "a pool to be placed must have a destination pool")]
        | vt == VTLocal -> $fromJustOrError [(loc, "meta.pool[.local] is not set")] =<< getMetaExprMayC [metaPoolLocal, metaPool]
        | vt == VTConst -> $fromJustOrError [(loc, "meta.pool[.const] is not set")] =<< getMetaExprMayC [metaPoolConst, metaPool]
        | vt == VTInline -> $fromJustOrError [(loc, "meta.pool[.code] is not set")] =<< getMetaExprMayC [metaPoolCode, metaPool]
        | otherwise -> $fromJustOrError [(loc, "meta.pool[.var] is not set")] =<< getMetaExprMayC [metaPoolVar, metaPool] -- VTVar
  (p', pVirt) <- getPoolElemRefC loc q
  setPositionC n (Just p', Left (minBound, maxBound))
  if | ty == poolType ->
        case v of
          Just vv ->
            applyMetaExprC vv >>= evaluateExprTopC >>= \case
              (_, E4Pointer _ vvn (TDPool True _ vnnVirt) 0) ->
                addPoolToPoolC loc (vvn, vnnVirt) n (p', pVirt) (if vt == VTConst then ByteValIsConst else ByteValIsInit)
              (_, ee) -> $throwFatalError [(loc, "pool initializer not found or not of type pool container: " ++ show ee)]
          Nothing -> $throwFatalError [(loc, "pool variable must have an initial value")]
     | vt == VTInline -> do
          v' <- mapM applyMetaExprC v
          size <- sizeOfTypeDefinitionC ty
          addInlineC n (size, v')
     | otherwise -> do -- VTConst | VTVar | VTLocal
          da <-
            case v of
              Just vv -> do
                vv' <- applyMetaExprC vv
                fn <- getCheck8C loc [metaCheckData8, metaCheckData, metaCheck]
                toByteValVectorC fn (bool ByteValIsInit ByteValIsConst (vt == VTConst)) ty vv'
              Nothing -> do
                size <- sizeOfTypeDefinitionC ty
                if vt == VTLocal && not flagDisableLocal
                  then do
                    ls <- getCallPathsForReferenceC n
                    return $ V.replicate (fromIntegral size) (ByteValLocal ls)
                  else
                    return $ V.replicate (fromIntegral size) (ByteValInit maxBound)
          addDataToPoolC
            loc
            (p', pVirt, locationOf q)
            ByteValPiece
              { bvpBytes = da
              , bvpAlign = al
              , bvpPage = pg
              , bvpNames = M.singleton n 0
              }
  return []

placeStmtInPoolC (S3CpuStmt l s) = cpuApplyMetaStmtC l s

placeIfC :: Cpu c => (Text, Expr3 c, Stmt3Block c) -> CSM3 c (Text, Expr4 c, Stmt4Block c)
placeIfC (t, e, blocks) = do
  b <- placeInPoolC blocks
  e' <- applyMetaExprC e
  return (t, e', b)

toByteValVectorC :: Cpu c => FunctionKey -> ConstOrInit -> TypeDefinition -> Expr4 c -> CSM3 c (Vector (ByteVal (Expr4 c)))
toByteValVectorC fn isConst TDByte e =
  return $ V.singleton (ByteValCode isConst (E4Function (locationOf e) fn [e]))
toByteValVectorC fn isConst (TDArray t s) e' = do
  elemSize <- fromIntegral <$> sizeOfTypeDefinitionC t
  (snd <$> evaluateExprTopC e') >>= \case
    (E4UserArray loc e fill) -> do
      let
        vLength = length e
      v <- foldM (\a b -> (a V.++) <$> toByteValVectorC fn isConst t b) V.empty e
      go loc v vLength elemSize fill
    (E4UserArrayBVS loc e fill) -> do
      let
        vLength = length e
        v = map (fromByteValSimple isConst) $ V.convert e
      go loc v vLength elemSize fill
    e -> $throwFatalError [(locationOf e, "toByteValVectorC is not fully defined: " ++ show t ++ " / " ++ showPrettySrc e)]
  where
    go loc v vLength elemSize fill =
      case (map fromIntegral s, fill) of
        (Nothing, _) -> return v
        (Just s', Nothing)
          | s' /= vLength -> $throwFatalError [(loc, "array-size-mismatch")]
          | otherwise -> return v
        (Just s', Just fill')
          | vLength > s' -> $throwFatalError [(loc, "array-size-mismatch")]
          | otherwise -> return $ v V.++ V.replicate ((s' - vLength) * elemSize) (ByteValCode isConst fill')
toByteValVectorC fn isConst (TDStruct s) e' =
  (snd <$> evaluateExprTopC e') >>= \case
    (E4UserStructOrUnion loc e fill) ->
      toByteValVectorStructC fn loc isConst s e fill >>= checkStructOrUnion loc
    e -> $throwFatalError [(locationOf e, "toByteValVectorC is not fully defined(1): " ++ show s ++ " / " ++ showPrettySrc e)]
toByteValVectorC fn isConst td@(TDUnion s) e' =
  (snd <$> evaluateExprTopC e') >>= \case
    (E4UserStructOrUnion loc e fill) -> do
      size <- sizeOfTypeDefinitionC td
      toByteValVectorUnionC fn loc isConst size s e fill >>= checkStructOrUnion loc
    e -> $throwFatalError [(locationOf e, "toByteValVectorC is not fully defined(1): " ++ show s ++ " / " ++ showPrettySrc e)]
toByteValVectorC _ _ t e = $throwFatalError [(locationOf e, "toByteValVectorC is not fully defined(2): " ++ show t ++ " / " ++ showPrettySrc e)]

checkStructOrUnion :: Cpu c => Location -> ([Vector (ByteVal (Expr4 c))], Map Text a) -> CSM3 c (Vector (ByteVal (Expr4 c)))
checkStructOrUnion loc (res, dat) = do
  unless (M.null dat) $
    $throwError [(loc, "toByteValVectorC: elements not used: " ++ intercalate ", " (map (unpack . fst) (M.toList dat)))]
  return $ V.concat res

toByteValVectorStructC :: Cpu c => FunctionKey -> Location -> ConstOrInit -> [(Maybe Text, TypeDefinition)] -> Map Text (Expr4 c) -> Maybe (Expr4 c) -> CSM3 c ([Vector (ByteVal (Expr4 c))], Map Text (Expr4 c))
toByteValVectorStructC fn loc constOrInit struct dat fill =
  foldM findData ([], dat) struct
  where
    findData (res, dta) (Just nam, td) = do
      size <- sizeOfTypeDefinitionC td
      case M.lookup nam dta of
        Just d -> do
          r <- toByteValVectorC fn constOrInit td d
          return (res ++ [r], nam `M.delete` dta)
        Nothing ->
          case fill of
            Just f ->
              return (res ++ [V.replicate (fromIntegral size) (ByteValCode constOrInit f)], dta)
            Nothing -> $throwFatalError [(loc, "toByteValVectorC: hole in struct")]
    findData (res, dta) (Nothing, td) = do
      size <- sizeOfTypeDefinitionC td
      case td of
        TDUnion union -> do
          (res', dta') <- toByteValVectorUnionC fn loc constOrInit size union dta fill
          return (res ++ res', dta')
        _ -> case fill of
          Just f ->
            return (res ++ [V.replicate (fromIntegral size) (ByteValCode constOrInit f)], dta)
          Nothing -> $throwFatalError [(loc, "toByteValVectorC: hole in struct")]

toByteValVectorUnionC :: Cpu c => FunctionKey -> Location -> ConstOrInit -> Int64 -> [(Maybe Text, TypeDefinition)] -> Map Text (Expr4 c) -> Maybe (Expr4 c) -> CSM3 c ([Vector (ByteVal (Expr4 c))], Map Text (Expr4 c))
toByteValVectorUnionC fn loc constOrInit size union dta fill =
  (catMaybes <$> mapM findData union) >>= \case
  [] ->
    case fill of
      Just f ->
        return ([V.replicate (fromIntegral size) (ByteValCode constOrInit f)], dta)
      Nothing -> $throwFatalError [(loc, "toByteValVectorC: hole in union")]
  [(res, td, dta')] -> do
    sizeTd <- sizeOfTypeDefinitionC td
    if sizeTd == size
      then return (res, dta')
      else case fill of
        Just f ->
          return (res ++ [V.replicate (fromIntegral size) (ByteValCode constOrInit f)], dta)
        Nothing -> $throwFatalError [(loc, "toByteValVectorC: hole in union")]
  _ -> $throwFatalError [(loc, "toByteValVectorC: multiple union sources")]
  where
    findData (Just nam, td) =
      case M.lookup nam dta of
        Just d -> do
          r <- toByteValVectorC fn constOrInit td d
          return $ Just ([r], td, nam `M.delete` dta)
        Nothing -> return Nothing
    findData (Nothing, td) =
      case td of
        TDStruct struct -> do
          (res, dta') <- toByteValVectorStructC fn loc constOrInit struct dta fill
          return $ Just (res, td, dta')
        _ -> return Nothing
