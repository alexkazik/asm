module Asm.Core.Phase4.PoolData
  ( reducePoolDataStateC
  , calcPoolDataLength
  ) where

import           Asm.Core.Prelude
import qualified Data.Map                           as M
import           Data.Proxy
import qualified Data.Set                           as S
import qualified Data.Vector                        as V

import           Asm.Core.Control.CompilerError
import           Asm.Core.Data.ByteVal
import           Asm.Core.Data.ByteValPiece
import           Asm.Core.Data.ByteValPiece.Combine
import           Asm.Core.Data.Cpu
import           Asm.Core.Data.CpuData
import           Asm.Core.Data.FunctionKey
import           Asm.Core.Data.KindDefinition
import           Asm.Core.Data.Reference
import           Asm.Core.Data.Ternary
import           Asm.Core.Data.TypeDefinition
import           Asm.Core.Flags
import           Asm.Core.Phase4.CompilerState4
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.Phase4.Data.MapExpr4
import           Asm.Core.Phase4.Data.PoolData
import           Asm.Core.Phase4.Data.Stmt4
import           Asm.Core.Phase4.Pool
import           Asm.Core.Phases34.Data.PoolState
import           Asm.Core.Phases34.EvaluateExpr
import           Asm.Core.PrettyPrint.Use
import           Asm.Data.InfInt64

reducePoolDataStateC :: Cpu c => (Reference, PoolData c) -> CSM4 c (Reference, PoolData c)
reducePoolDataStateC (poolName, PoolDataStartFlat{stateStart, stateFlat, stateData, statePool})
  | null stateStart && null statePool = do
      setHasChangedC
      sb <- cpuOptimiseGlobalStmtBlockC stateFlat
      let da = map cpuStmtBlockToByteValPiece sb
      return (poolName, PoolDataOptimised{stateOpt = S.fromList da <> stateData, stateFinal = S.empty})
  | otherwise = do
      (stSt, stFl) <- partitionEithers <$> mapM reducePoolDataStateStartC stateStart
      stOpFl' <- mapM conv5 stFl
      stOpFl <- mapM cpuOptimiseLocalStmtBlockC stOpFl'
      (plPl, plDa) <- partitionEithers <$> mapM reducePoolPoolStateStartC statePool
      return (poolName, PoolDataStartFlat{stateStart = stSt, stateFlat = stOpFl ++ stateFlat, stateData = stateData <> S.fromList plDa, statePool = plPl})
reducePoolDataStateC ds@(poolName, PoolDataOptimised{stateOpt, stateFinal}) = go =<< getPoolStateC poolName
  where
    go poolState
      | null stateOpt && InfInt64 (psStartLow poolState) /= psStartHigh poolState = return ds -- can't be placed yet, wait
      | null stateOpt = do
          setHasChangedC
          return (poolName, PoolDataFinal $ combine (psStartLow poolState) $ S.toList stateFinal)
      | otherwise = do
          (opOp, opFi) <- partitionEithers <$> mapM reducePoolDataStateOptC (S.toList stateOpt)
          isChanged <- getHasChangedC
          if isChanged
            then return (poolName, PoolDataOptimised{stateOpt = S.fromList opOp, stateFinal = stateFinal <> S.fromList opFi})
            else do
              (o, f) <- foldlM workDefaultC (S.empty, stateFinal <> S.fromList opFi) opOp
              return (poolName, PoolDataOptimised{stateOpt = o, stateFinal = f})

reducePoolDataStateC (poolName, PoolDataFinal bvp) = do
  bvp' <- reducePoolDataStateFinalC bvp
  return (poolName, PoolDataFinal bvp')

conv5 :: Cpu c => Stmt4Block c ->  CSM4 c (CS5Block c)
conv5 block = concat <$> mapM convert5 block

convert5 :: Cpu c => Stmt4 c ->  CSM4 c (CS5Block c)
convert5 (S4LabelDefinition loc x) = cpuLabelDefinitionStmt5C loc x
convert5 stmt@S4IfBlock{}          = $throwFatalError [(locationOf stmt, "found if block in optimisation step")]
convert5 stmt@S4For{}              = $throwFatalError [(locationOf stmt, "found for loop in optimisation step")]
convert5 (S4CpuStmt loc cs)        = cpuConvertToStmt5C loc cs

workDefaultC :: Cpu c => (Set (ByteValPiece (Expr4 c)), Set (ByteValPiece (Expr4 c))) -> ByteValPiece (Expr4 c) -> CSM4 c (Set (ByteValPiece (Expr4 c)), Set (ByteValPiece (Expr4 c)))
workDefaultC (o, f) bvp@ByteValPiece{bvpBytes} = do
  let
    work = V.foldl' (\c b -> if isCode b then c else c+1) 0 bvpBytes % V.length bvpBytes
  doWork <- getUseDefaultC
  if Just work == doWork
    then do
      resetUseDefaultC
      setHasChangedC
      when flagDebugCompiler
        (traceM $ "forced BVP: " ++ show bvp)
      return (o, S.insert bvp f)
    else do
      setDefaultC work
      return (S.insert bvp o, f)

isCode :: ByteVal c -> Bool
isCode (ByteValCode _ _) = True
isCode _                 = False


reducePoolDataStateStartC :: Cpu c => Stmt4Block c -> CSM4 c (Either (Stmt4Block c) (Stmt4Block c))
reducePoolDataStateStartC block = do
  setMetaIsFlatC True
  block' <- reduceStmtBlockC block
  isFlat <- getMetaIsFlatC
  if isFlat
    then return $ Right block'
    else return $ Left block'

reduceStmtBlockC :: Cpu c => Stmt4Block c -> CSM4 c (Stmt4Block c)
reduceStmtBlockC block = concat <$> mapM reduceStmtC block

reducePoolPoolStateStartC :: Cpu c => (Reference, Reference, ConstOrInit) -> CSM4 c (Either (Reference, Reference, ConstOrInit) (ByteValPiece (Expr4 c)))
reducePoolPoolStateStartC (s, d, constOrInit) =
  getPoolDataC s >>= \case
    PoolDataFinal bvp -> do
      setHasChangedC
      return $ Right $ bvp{bvpNames = M.singleton d 0, bvpBytes = map (byteValSetConstOrInit constOrInit) $ bvpBytes bvp}
    _ -> return $ Left (s, d, constOrInit)


reduceStmtC :: Cpu c => Stmt4 c -> CSM4 c (Stmt4Block c)

reduceStmtC (S4IfBlock _ []) = setHasChangedC *> return []

reduceStmtC (S4IfBlock loc ((l,e,b):xs)) = do
  (ty, ne) <- evaluateExprTopC e
  if ty /= KDData TDBool
    then do
      $throwError [(loc, "if needs a bool as argument in " ++ showPrettySrc (ty, ne))]
      setHasChangedC *> return []
    else
      case ne of
        (E4ConstBool _ True) -> setHasChangedC *> reduceStmtBlockC b
        (E4ConstBool _ False) -> setHasChangedC *> reduceStmtC (S4IfBlock loc xs)
        _ -> do
          setMetaIsFlatC False
          return [S4IfBlock loc ((l,ne,b):xs)]

reduceStmtC x@S4CpuStmt{} = return [x]

reduceStmtC (S4For loc var from cmp to step block) = do
  (fromTy, from') <- evaluateExprC from
  (toTy, to') <- evaluateExprC to
  if | fromTy /= KDData TDInt -> do
         $throwError [(loc, "for needs a int as 'from' argument")]
         setHasChangedC *> return []
     | toTy /= KDData TDInt -> do
         $throwError [(loc, "for needs a int as 'to' argument")]
         setHasChangedC *> return []
     | otherwise ->
          case (conv from', conv to') of
            ((fromExpr, Just (fromLo, fromHi)), (toExpr, Just (toLo, toHi)))
              | toLo == maxBound -> do
                  $throwError [(loc, "for: 'to' argument needs to be less than max int")]
                  setHasChangedC *> return []
              | doCmp fromHi cmp toLo ->
                  trace ("for:range:gen: " ++ show (fromLo, fromHi) ++ showCmp cmp ++ show (toLo, toHi)) $
                  setMetaIsFlatC False *> setHasChangedC *> reduceStmtBlockC (genBlock ++ [S4For loc var (E4Function loc opPLUS [fromExpr, genStep]) cmp toExpr step block])
              | doRevCmp fromLo cmp toHi ->
                  trace ("for:range:end: " ++ show (fromLo, fromHi) ++ showCmp cmp ++ show (toLo, toHi)) $
                  setHasChangedC *> return []
              | otherwise ->
                  trace ("for:range:wait: " ++ show (fromLo, fromHi) ++ showCmp cmp ++ show (toLo, toHi)) $
                  setMetaIsFlatC False *> return [S4For loc var fromExpr cmp toExpr step block]
              where
                genBlock = mapExprInStmtBlock (replaceLoopVar var fromExpr) block
                genStep = replaceLoopVar var fromExpr step
            ((fromExpr, _), (toExpr, _)) ->
              trace ("for:unknown: " ++ show fromExpr ++ showCmp cmp ++ show toExpr) $ setMetaIsFlatC False >> return [S4For loc var fromExpr cmp toExpr step block]
  where
    conv (E4RangedInt _ l h _ e) = (e, Just (l, h))
    conv e@(E4ConstInt _ i)      = (e, Just (InfInt64 i, InfInt64 i))
    conv e                       = (e, Nothing)
    doCmp l ForCmpLessThan r  = l < r
    doCmp l ForCmpLessEqual r = l <= r
    doRevCmp l ForCmpLessThan r  = l >= r
    doRevCmp l ForCmpLessEqual r = l > r
    showCmp ForCmpLessThan  = " < "
    showCmp ForCmpLessEqual = " <= "

reduceStmtC x@S4LabelDefinition{} = return [x]


replaceLoopVar :: Cpu c => Reference -> Expr4 c -> Expr4 c -> Expr4 c
replaceLoopVar n i (E4LoopVariable _ n')
  | n == n' = i
replaceLoopVar n i x = mapExpr (replaceLoopVar n i) x

reducePoolDataStateOptC :: Cpu c => ByteValPiece (Expr4 c) -> CSM4 c (Either (ByteValPiece (Expr4 c)) (ByteValPiece (Expr4 c)))
reducePoolDataStateOptC bvp@ByteValPiece{bvpBytes} = do
  setMetaIsFlatC True
  newBytes <- mapM reducePoolDataStateOptFlatC bvpBytes
  isFlat <- getMetaIsFlatC
  if isFlat
    then return $ Right $ bvp{bvpBytes=newBytes}
    else return $ Left $ bvp{bvpBytes=newBytes}

reducePoolDataStateFinalC :: Cpu c => ByteValPiece (Expr4 c) -> CSM4 c (ByteValPiece (Expr4 c))
reducePoolDataStateFinalC bvp@ByteValPiece{bvpBytes} = do
  newBytes <- mapM reducePoolDataStateOptFlatC bvpBytes
  return bvp{bvpBytes=newBytes}

reducePoolDataStateOptFlatC :: Cpu c => ByteVal (Expr4 c) -> CSM4 c (ByteVal (Expr4 c))
reducePoolDataStateOptFlatC (ByteValCode constOrInit (E4ConstInt _ i)) =
  setHasChangedC *> return (byteValWord8 constOrInit (fromIntegral i))
reducePoolDataStateOptFlatC (ByteValCode constOrInit (E4ConstMaskedInt _ mi)) =
  setHasChangedC *> return (byteValMaskedWord8 constOrInit (fromIntegral v) (fromIntegral m))
  where
    (v, m) = tValueAndMask mi
reducePoolDataStateOptFlatC (ByteValCode constOrInit (E4ByteVal _ bv)) =
  setHasChangedC *> return (byteValSetConstOrInit constOrInit bv)
reducePoolDataStateOptFlatC (ByteValCode constOrInit e) = do
  setMetaIsFlatC False
  (_k, e') <- evaluateExprTopC e
  return $ ByteValCode constOrInit e'
reducePoolDataStateOptFlatC x = return x


calcPoolDataLength :: forall c. Cpu c => PoolData c -> (Int64, InfInt64)
calcPoolDataLength PoolDataStartFlat{stateStart, stateFlat, stateData, statePool} =
  let
    proxy = Proxy :: Proxy c
  in
    if null statePool
      then calcLen2 (map (stmtBLen proxy) stateStart ++ map (stmt5BLen proxy) stateFlat ++ map bvpLen (S.toList stateData))
      else (0, maxBound)

calcPoolDataLength PoolDataOptimised{stateOpt, stateFinal} =
  calcLen2 (map bvpLen (S.toList stateOpt) ++ map bvpLen (S.toList stateFinal))

calcPoolDataLength (PoolDataFinal p) = (l, InfInt64 l)
  where
    l = fromIntegral $ V.length (bvpBytes p)

calcLen2 :: [(Int64, InfInt64, Int64, Maybe Int64)] -> (Int64, InfInt64)
calcLen2 = foldl' calcLen2' (0,0)

calcLen2' :: (Int64, InfInt64) -> (Int64, InfInt64, Int64, Maybe Int64) -> (Int64, InfInt64)
calcLen2' (mi, ma) (emi, ema, eal, epa) = (max mi emi, ma + ema + (InfInt64 eal - 1) + maybe 0 InfInt64 epa)

stmtBLen :: Cpu c => proxy c -> Stmt4Block c -> (Int64, InfInt64, Int64, Maybe Int64)
stmtBLen proxy block = (minLen, maxLen, cpuCodeAlign proxy, Nothing)
  where
    (minLen, maxLen) = foldl' stmtLen (0,0) block

stmtLen :: Cpu c => (Int64, InfInt64) -> Stmt4 c -> (Int64, InfInt64)
stmtLen ab (S4IfBlock _ bs) = (minimumEx $ map fst bln, maximumEx $ map snd bln)
  where
    bln = map (stmtBLen2 . (\(_,_,b) -> b)) bs
    stmtBLen2 = foldl' stmtLen ab
stmtLen x S4LabelDefinition{} = x
stmtLen (a, b) st@(S4CpuStmt _ c) = (a+aa, b+InfInt64 bb)
  where
    (aa,bb) = cpuStmtMinMaxLength st c
stmtLen (a, _) S4For{} = (a, maxBound)

stmt5BLen :: Cpu c => proxy c -> CS5Block c -> (Int64, InfInt64, Int64, Maybe Int64)
stmt5BLen proxy block = (minLen, maxLen, cpuCodeAlign proxy, Nothing)
  where
    (minLen, maxLen) = foldl' (stmt5Len proxy) (0,0) block

stmt5Len :: Cpu c => proxy c -> (Int64, InfInt64) -> CS5 c -> (Int64, InfInt64)
stmt5Len proxy (a, b) st = (a+aa, b+InfInt64 bb)
  where
    (aa,bb) = cpuStmt5MinMaxLength proxy st

bvpLen :: ByteValPiece (Expr4 e) -> (Int64, InfInt64, Int64, Maybe Int64)
bvpLen ByteValPiece{bvpBytes, bvpAlign, bvpPage} =
  (len, InfInt64 len, bvpAlign, bvpPage)
  where
    len = fromIntegral $ V.length bvpBytes
