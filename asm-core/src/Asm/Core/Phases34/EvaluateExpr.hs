{-# LANGUAGE TypeFamilies #-}

module Asm.Core.Phases34.EvaluateExpr
  ( evaluateExprC
  , evaluateExprTopC
  ) where

import           Asm.Core.Prelude
import qualified Data.Map                               as M

import           Asm.Core.Data.Cpu
import           Asm.Core.Data.FunctionKey
import           Asm.Core.Data.KindDefinition
import           Asm.Core.Data.TypeDefinition
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.Phases34.Data.CompilerState34
import           Asm.Core.Phases34.Data.Function
import           Asm.Core.Phases34.TypeDefinition
import           Asm.Core.PrettyPrint.Use
import           Asm.Core.SourcePos


removePartialExpr :: (KindDefinition, Expr4 e) -> (KindDefinition, Expr4 e)
removePartialExpr (k, E4RangedInt _ _ _ _ s) = (k, s)
removePartialExpr ke                         = ke

evaluateExprTopC :: (CSM34 m, Cpu c, CSM34Cpu m ~ c) => Expr4 c -> m (KindDefinition, Expr4 c)
evaluateExprTopC e = removePartialExpr <$> evaluateExprC e

evaluateExprC :: (CSM34 m, Cpu c, CSM34Cpu m ~ c) => Expr4 c -> m (KindDefinition, Expr4 c)

evaluateExprC (E4NamespaceRef loc l) = printErrorC $ (loc, "ENamespaceRef: " ++ show l):[sourcePos||]
evaluateExprC x@(E4LoopVariable loc l) = do
  isPhase4 <- isPhase4C
  bool
    (return (KDData TDInt, x))
    (printErrorC $ (loc, "ELoopVariable: " ++ show l):[sourcePos||])
    isPhase4
evaluateExprC x@(E4Meta _ m) = return (KDMeta m, x)
evaluateExprC x@E4ConstBool{} = return (KDData TDBool, x)
evaluateExprC x@E4ConstInt{} = return (KDData TDInt, x)
evaluateExprC x@E4ConstMaskedInt{} = return (KDData TDMaskedInt, x)
evaluateExprC (E4Function loc f p) = evaluateFunctionC' loc f p

evaluateExprC (E4DerefArray loc a e) = do
  a' <- evaluateExprC a
  e' <- evaluateExprC e
  evaluateDerefArrayC loc a' e'

evaluateExprC (E4DefineArray loc a e) = do
  a' <- evaluateExprC a
  e' <- mapM evaluateExprC e
  evaluateDefineArrayC loc a' e'

evaluateExprC (E4DerefStruct loc e s) = evaluateDerefStructC loc s =<< evaluateExprC e

evaluateExprC x@(E4Pointer _ _ t _o) = return (KDPointer t, x)

evaluateExprC x@(E4Type _ t _o) = return (KDType t, x)

evaluateExprC x@E4UserArray{} = return (KDUserArray, x)
evaluateExprC x@E4UserArrayBVS{} = return (KDUserArray, x)

evaluateExprC x@E4UserStructOrUnion{} = return (KDUserStructOrUnion, x)

evaluateExprC (E4TypeStruct loc str) = do
  str' <- mapM evalStr str
  let t = TDStruct str'
  return (KDType t, E4Type loc t 0)
  where
    evalStr (n,t) = do
      let na = n
      t' <- evaluateExprC t
      case t' of
        (KDType td, _) -> return (na, td)
        (a,b)              -> printErrorC $ (loc, "evaluateExprC ETypeStruct no type: " ++ showPretty a ++ "; " ++ showPrettySrc b):[sourcePos||]

evaluateExprC x@E4ByteVal{} = return (KDData TDByte, x)

evaluateExprC x@E4RangedInt{} = printErrorC $ (locationOf x, "evaluateExprC: don't know how to handle " ++ showPrettySrc x):[sourcePos||]
evaluateExprC x@E4CpuExpr{} = printErrorC $ (locationOf x, "evaluateExprC: don't know how to handle " ++ showPrettySrc x):[sourcePos||]
evaluateExprC x@E4TypeUnion{} = printErrorC $ (locationOf x, "evaluateExprC: don't know how to handle " ++ showPrettySrc x):[sourcePos||]
evaluateExprC x@E4MagicValue{} = return (KDMagicValue, x)


evaluateFunctionC' :: (CSM34 m, Cpu c, CSM34Cpu m ~ c) => Location -> FunctionKey -> [Expr4 c] -> m (KindDefinition, Expr4 c)
evaluateFunctionC' loc f p = evaluateFunctionC loc f =<< mapM evaluateExprC p

evaluateFunctionC :: forall m c. (CSM34 m, Cpu c, CSM34Cpu m ~ c) => Location -> FunctionKey -> [(KindDefinition, Expr4 c)] -> m (KindDefinition, Expr4 c)
evaluateFunctionC loc f es = do
  fns <- lookupFunctionC f
  reduceFunctionsC fns Nothing >>= \case
    Just (FnrResult r) -> setHasChangedC *> return r
    Just (FnrRangedInt l h no) -> return (KDData TDInt, E4RangedInt loc l h no reconstruct)
    Just (FnrUnchanged k) -> return (k, reconstruct)
    Just FnrNoMatch -> printErrorC $ (loc, "function/operator " ++ show f ++ " is known but the argument count/type mismatch: " ++ show (map (showKdAsParam . fst) es)):[sourcePos||]
    Nothing -> printErrorC $ (loc, "function/operator " ++ show f ++ " is not known: " ++ show (map (showKdAsParam . fst) es)):[sourcePos||]
  where
    reduceFunctionsC :: [Location -> [(KindDefinition, Expr4 c)] -> m (FunctionResult c)] -> Maybe (FunctionResult c) -> m (Maybe (FunctionResult c))
    reduceFunctionsC [] w = return w
    reduceFunctionsC (fn:fns') _ =
      fn loc es >>= \case
        FnrNoMatch -> reduceFunctionsC fns' (Just FnrNoMatch)
        r -> return (Just r)
    reconstruct = E4Function loc f $ map (snd . removePartialExpr) es


evaluateDerefArrayC :: (CSM34 m, Cpu c, CSM34Cpu m ~ c) => Location -> (KindDefinition, Expr4 c) -> (KindDefinition, Expr4 c) -> m (KindDefinition, Expr4 c)
evaluateDerefArrayC loc (KDUserArray, E4UserArray _ arr _) (KDData TDInt, E4ConstInt _ i) =
  case index arr (fromIntegral i) of
    Just e  -> evaluateExprC e
    Nothing -> printErrorC $ (loc, "index " ++ show i ++ " out of range"):[sourcePos||]
evaluateDerefArrayC loc (KDPointer _, E4Pointer _ name (TDArray t size) offset) (KDData TDInt, E4ConstInt _ i) =
  if i < 0 || maybe False (i >=) size
    then printErrorC $ (loc, "index " ++ show i ++ " out of range"):[sourcePos||]
    else do
      elemSize <- sizeOfTypeDefinitionC t
      return (KDPointer t, E4Pointer loc name t (offset + elemSize * i))
evaluateDerefArrayC l (a@(KDPointer _), b@(E4Pointer _ _ (TDArray t _) _)) (KDData TDInt, d) = do
  isPhase4 <- isPhase4C
  if isPhase4
    then printErrorC $ (l, "evaluateDerefArrayC(1) " ++ showPretty a ++ "; " ++ showPrettySrc b ++ "\n" ++ showPrettySrc d):[sourcePos||]
    else return (KDPointer t, E4DerefArray l b d)
evaluateDerefArrayC l (KDUserArray, _) _ = printErrorC $ (l, "Array offset must be of type int"):[sourcePos||]
evaluateDerefArrayC l (a,b) (c,d) = printErrorC $ (l, "evaluateDerefArrayC(2) " ++ showPretty a ++ "; " ++ showPrettySrc b ++ "\n" ++ showPretty c ++ "; " ++ showPrettySrc d):[sourcePos||]


evaluateDefineArrayC :: (CSM34 m, Cpu c, CSM34Cpu m ~ c) => Location -> (KindDefinition, Expr4 c) -> Maybe (KindDefinition, Expr4 c) -> m (KindDefinition, Expr4 c)
evaluateDefineArrayC loc (KDType t, _) (Just (KDData TDInt, E4ConstInt _ i)) = do
  let at = TDArray t (Just i)
  if i >= 0
    then return (KDType at, E4Type loc at 0)
    else printErrorC $ (loc, "Array size must be positive"):[sourcePos||]
evaluateDefineArrayC loc (KDType t, _) Nothing = do
  let at = TDArray t Nothing
  return (KDType at, E4Type loc at 0)
evaluateDefineArrayC l (a,b) (Just (KDData TDInt, _)) = printErrorC $ (l, "evaluateDefineArrayC " ++ showPretty a ++ "; " ++ showPrettySrc b):[sourcePos||]
evaluateDefineArrayC l (KDType _t, _) _ = printErrorC $ (l, "Array size must be of type int"):[sourcePos||]
evaluateDefineArrayC l (a,b) Nothing = printErrorC $ (l, "evaluateDefineArrayC " ++ showPretty a ++ "; " ++ showPrettySrc b):[sourcePos||]
evaluateDefineArrayC l (a,b) (Just (c,d)) = printErrorC $ (l, "evaluateDefineArrayC " ++ showPretty a ++ "; " ++ showPrettySrc b ++ "\n" ++ showPretty c ++ "; " ++ showPrettySrc d):[sourcePos||]




evaluateDerefStructC :: (CSM34 m, Cpu c, CSM34Cpu m ~ c) => Location -> Text -> (KindDefinition, Expr4 c) -> m (KindDefinition, Expr4 c)

evaluateDerefStructC loc element (KDPointer (TDStruct str), E4Pointer _ lab _t ofs) =
  foldM findElem ([], ofs) str >>= \case
    ([(newTy, newOfs)], _) ->
      return (KDPointer newTy, E4Pointer loc lab newTy newOfs)
    ([], _) ->
      printErrorC $ (loc, "1/can't dereference variable " ++ show lab ++ ", elementent " ++ show element ++ " was not found"):[sourcePos||]
    _ ->
      printErrorC $ (loc, "1/can't dereference variable " ++ show lab ++ ", elementent " ++ show element ++ " was not unique"):[sourcePos||]
  where
    findElem (res, newOfs) (Just na, newTy) = do
      s <- sizeOfTypeDefinitionC newTy
      let
        nextOfs = newOfs + s
      if na == element
        then return ((newTy, newOfs):res, nextOfs)
        else return (res, nextOfs)
    findElem (res, newOfs) (Nothing, newTy) = do
      s <- sizeOfTypeDefinitionC newTy
      let
        nextOfs = newOfs + s
      case pickUnnamedUnionElement newTy of
            Just uniTy -> return ((uniTy, newOfs):res, nextOfs)
            Nothing    -> return (res, nextOfs)
    pickUnnamedUnionElement (TDUnion uni) = lookup (Just element) uni
    pickUnnamedUnionElement _             = Nothing

evaluateDerefStructC loc element (KDPointer (TDUnion str), E4Pointer _ lab _t ofs) =
  case lookup (Just element) str of
    Just newTy -> return (KDPointer newTy, E4Pointer loc lab newTy ofs)
    Nothing -> printErrorC $ (loc, "2/can't dereference variable " ++ show lab ++ ", elementent " ++ show element ++ " was not found"):[sourcePos||]

evaluateDerefStructC loc element (xa@(KDType (TDStruct str)), xb@(E4Type _ _t ofs)) =
  case lookup (Just element) str of
    Just newTy -> do
      newOfs <- foldM (\a (_,b) -> (+) a <$> sizeOfTypeDefinitionC b) 0 (takeWhile (\(a,_) -> a /= Just element) str)
      return (KDPointer newTy, E4Type loc newTy (ofs+newOfs))
    Nothing -> printErrorC $ (loc, "a1/can't dereference variable " ++ showPretty xa ++ "; " ++ showPrettySrc xb ++ ", elementent " ++ show element ++ " was not found"):[sourcePos||]

evaluateDerefStructC loc element (a@(KDType (TDUnion str)), b@(E4Type _ _t ofs)) =
  case lookup (Just element) str of
    Just newTy -> return (KDPointer newTy, E4Type loc newTy ofs)
    Nothing -> printErrorC $ (loc, "a2/can't dereference variable " ++ showPretty a ++ "; " ++ showPrettySrc b ++ ", elementent " ++ show element ++ " was not found"):[sourcePos||]

evaluateDerefStructC loc element (a@KDUserStructOrUnion, b@(E4UserStructOrUnion _ es _)) =
  case M.lookup element es of
    Just e -> evaluateExprC e
    Nothing -> printErrorC $ (loc, "b1/can't dereference variable " ++ showPretty a ++ "; " ++ showPrettySrc b ++ ", elementent " ++ show element ++ " was not found"):[sourcePos||]

evaluateDerefStructC loc _element (typ, lab) =
  printErrorC $ (loc, "a4/can't dereference " ++ showPrettySrc lab ++ " of type " ++ showPretty typ):[sourcePos||]
