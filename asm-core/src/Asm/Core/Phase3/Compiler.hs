module Asm.Core.Phase3.Compiler
  ( compile3
  ) where

import           Asm.Core.Prelude
import qualified Data.Map                            as M

import           Asm.Core.Data.Cpu
import           Asm.Core.Data.Reference
import           Asm.Core.Phase2.Data.CompilerState2
import           Asm.Core.Phase3.CallPath
import           Asm.Core.Phase3.CompilerState3
import           Asm.Core.Phase3.Data.Stmt3
import           Asm.Core.Phase3.PlaceInPool
import           Asm.Core.Phases.Data.PoolDefinition
import           Asm.Core.Phases34.Function
import           Asm.Core.SourcePos
import           Asm.Data.InfInt64


compile3 :: Cpu c => (Stmt3Block c, CompilerState2 c) -> CompilerState3 c
compile3 (x2, s2) =
  let
    ((), s3) = runState go (initialState3 s2 functionKeyMap)
  in
    s3
    -- printErrorS s3 []
  where
    go = do
      mapM_ convertCallPathC (M.toList $ cs2CallPaths s2)
      state (\s -> ((), s{cs3CallPaths = mergeCallPaths (cs3CallPaths s)}))
      placeInPoolC x2 >>= \case
        [] -> return ()
        (firstStmt:_) -> printErrorC $
            (locationOf firstStmt, "Code at top level") :
            [sourcePos||]
      mapM_ setLowerBoundC =<< M.toList <$> getPositionsC

setLowerBoundC :: Cpu c => (Reference, (Maybe Reference, Either (InfInt64, InfInt64) Int64)) -> CSM3 c ()
setLowerBoundC (name, (Just poolName, Left (lo, hi))) =
  -- this function may short cut some calculations but is not required
  getPoolDefinitionC poolName >>= \case
    Just pd -> setPositionC name (Just poolName, Left (lo `max` InfInt64 (pdStart pd), hi))
    Nothing -> return ()
setLowerBoundC _ = return ()
