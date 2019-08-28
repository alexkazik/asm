{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Asm.Core.Phase4.PoolState
  ( calcPoolStateC
  ) where

import           Asm.Core.Prelude
import qualified Data.Map.Strict                     as M
import           Data.Proxy

import           Asm.Core.Control.CompilerError
import           Asm.Core.Data.ByteValPiece
import           Asm.Core.Data.Cpu
import           Asm.Core.Data.CpuData
import           Asm.Core.Data.Reference
import           Asm.Core.Phase4.CompilerState4
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.Phase4.Data.PoolData
import           Asm.Core.Phase4.Data.Stmt4
import           Asm.Core.Phase4.Pool
import           Asm.Core.Phase4.PoolData
import           Asm.Core.Phases.Data.PoolDefinition
import           Asm.Core.Phases34.Data.PoolState
import           Asm.Core.PrettyPrint.Use
import           Asm.Data.InfInt64

calcPoolStateC :: Cpu c => CSM4 c ()
calcPoolStateC = mapM_ calcOuterC =<< getPoolDefinitionsC

calcOuterC :: Cpu c => PoolDefinition -> CSM4 c ()
calcOuterC PoolDefinition{pdStart, pdPools} = calcInnerC pdStart (InfInt64 pdStart) pdPools

calcInnerC :: Cpu c => Int64 -> InfInt64 -> [Reference] -> CSM4 c ()
calcInnerC _ _ [] = return ()
calcInnerC startLow startHigh (p:ps) = do
  poolD <- getPoolDataC p
  let (lL, lH) = calcPoolDataLength poolD
  -- debug
  sta <- getPoolStateC p
  when (lH == 0) $
    $throwFatalError [([], "length to zero " ++ show (p, lL, lH, sta) ++ " ; " ++ showPretty poolD)]
  when (lL < psLengthLow sta || lH > psLengthHigh sta) $
    $throwFatalError [([], "length mismatch " ++ show (p, lL, lH, sta) ++ " ; " ++ showPretty poolD)]
  -- /debug
  setPoolStateC
    p
    PoolState
      { psStartLow = startLow
      , psStartHigh = startHigh
      , psLengthLow = lL
      , psLengthHigh = lH
      }
  updatePositionC (Just p, (InfInt64 startLow, startHigh + lH - 1)) poolD
  if InfInt64 startLow == startHigh
    then setPositionC p (Just p, Right startLow)
    else setPositionC p (Just p, Left (InfInt64 startLow, startHigh))
  calcInnerC (startLow+lL) (startHigh+lH) ps


updatePositionC :: forall c. Cpu c => (Maybe Reference, (InfInt64, InfInt64)) -> PoolData c -> CSM4 c ()
updatePositionC p PoolDataStartFlat{stateStart, stateFlat, stateData} = do
  mapM_ (updateStmtBlockC p) stateStart
  mapM_ (updateStmt5BlockC (Proxy :: Proxy c) p) stateFlat
  mapM_ (updateBVPC p) stateData
updatePositionC p PoolDataOptimised{stateOpt, stateFinal} = do
  mapM_ (updateBVPC p) stateOpt
  mapM_ (updateBVPC p) stateFinal
updatePositionC (po, _) (PoolDataFinal ByteValPiece{bvpNames}) =
  mapM_ (\(n,p) -> setPositionC n (po, Right p)) (M.toList bvpNames)

updateBVPC :: Cpu c => (Maybe Reference, (InfInt64, InfInt64)) -> ByteValPiece (Expr4 e) -> CSM4 c ()
updateBVPC p ByteValPiece{bvpNames} = mapM_ (`setPositionLeftC` p) (M.keys bvpNames)

updateStmtBlockC :: Cpu c => (Maybe Reference, (InfInt64, InfInt64)) -> Stmt4Block c -> CSM4 c ()
updateStmtBlockC p = mapM_ (updateStmtC p)

updateStmtC :: Cpu c => (Maybe Reference, (InfInt64, InfInt64)) -> Stmt4 c-> CSM4 c ()
updateStmtC p (S4LabelDefinition _ n) = setPositionLeftC n p
updateStmtC p (S4IfBlock _ blks)      = mapM_ (\(_,_,b) -> updateStmtBlockC p b) blks
updateStmtC p st@(S4CpuStmt _ s)      = mapM_ (`setPositionLeftC` p) (cpuStmtInlineNames st s)
updateStmtC _ S4For{}                 = return ()

updateStmt5BlockC :: Cpu c => proxy c -> (Maybe Reference, (InfInt64, InfInt64)) -> CS5Block c -> CSM4 c ()
updateStmt5BlockC proxy p = mapM_ (updateStmt5C proxy p)

updateStmt5C :: Cpu c => proxy c -> (Maybe Reference, (InfInt64, InfInt64)) -> CS5 c -> CSM4 c ()
updateStmt5C proxy p st     = mapM_ (`setPositionLeftC` p) (cpuStmt5InlineNames proxy st)

setPositionLeftC :: Cpu c => Reference -> (Maybe Reference, (InfInt64, InfInt64)) -> CSM4 c ()
{-# INLINE setPositionLeftC #-}
setPositionLeftC n (t, v) = setPositionC n (t, Left v)
