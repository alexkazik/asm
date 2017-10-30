module Asm.Core.Phase3.CompilerState3
  ( module Asm.Core.Phase3.Data.CompilerState3
  , module Asm.Core.Phase3.CompilerState3
  , module Asm.Core.Phases34.Data.CompilerState34
  , module Asm.Core.Phases.Data.CompilerState1234
  ) where

import           Asm.Core.Prelude
import qualified Data.Map.Strict                        as M
import qualified Data.Set                               as S

import           Asm.Core.Data.Cpu
import           Asm.Core.Data.FunctionKey
import           Asm.Core.Data.KindDefinition
import           Asm.Core.Data.MetaKey
import           Asm.Core.Data.Reference
import qualified Asm.Core.Data.Tree                     as R
import           Asm.Core.Phase2.Data.CompilerState2
import           Asm.Core.Phase3.Data.CompilerState3
import           Asm.Core.Phase3.Data.Expr3
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.Phase4.Data.PoolData
import           Asm.Core.Phases.Data.CompilerState1234
import           Asm.Core.Phases.Data.PoolDefinition
import           Asm.Core.Phases34.Data.CompilerState34
import           Asm.Core.Phases34.Data.Function
import           Asm.Core.Phases34.Data.PoolState
import           Asm.Core.SourcePos
import           Asm.Data.InfInt64

initialReader3 :: Cpu c => CompilerReader2 c -> CompilerState2 c -> CompilerWriter2 c -> FunctionKeyMap [Function (CSM3 c) c] -> CompilerReader3 c
initialReader3 CRd2{..} CSt2{..} CWr2{..} fns =
  CRd3
    { cs3PoolDefinition = cs2PoolDefinition
    , cs3PoolState = foldl' (createPools emptyPoolState) M.empty cs2PoolDefinition
    , cs3TypeInExpr = cs2TypeInExpr
    , cs3Functions = fns
    }
  where
    createPools :: a -> Map Reference a -> PoolDefinition -> Map Reference a
    createPools def pools pd = foldl' (createPool def) pools (pdPools pd)
    createPool :: a -> Map Reference a -> Reference -> Map Reference a
    createPool def pools na = M.insert na def pools

initialState3 :: Cpu c => CompilerReader2 c -> CompilerState2 c -> CompilerWriter2 c -> CompilerState3 c
initialState3 CRd2{..} CSt2{..} CWr2{..} =
  CSt3
    { cs3Data = cs2Data
    , cs3MetaData = cpuDefaultMetaData
    , cs3MetaStickyData = mksEmpty
    , cs3Position = cs2Position
    , cs3PoolData = foldl' (createPools ep) M.empty cs2PoolDefinition
    , cs3CallPaths = M.empty
    }
  where
    createPools :: a -> Map Reference a -> PoolDefinition -> Map Reference a
    createPools def pools pd = foldl' (createPool def) pools (pdPools pd)
    createPool :: a -> Map Reference a -> Reference -> Map Reference a
    createPool def pools na = M.insert na def pools
    ep :: (PoolData c)
    ep = PoolDataStartFlat{stateStart=[], stateFlat=[], stateData=S.empty, statePool=[]}


addNameC :: Cpu c => Text -> CSM3 c Reference
addNameC name = state go
  where
    go s@CSt3{..} =
      case R.insertUnlinked R.root name ([], KDCpu) cs3Data of
        (path, tree) -> (path, s{cs3Data = tree})

getTypeInExprC :: Cpu c => Reference -> CSM3 c (Expr3 c)
getTypeInExprC k = asks (\s -> (cs3TypeInExpr s M.! k))


getKindC :: Cpu c => Reference -> CSM3 c (Location, KindDefinition)
getKindC i = state (\s -> (R.get i (cs3Data s), s))

getPositionsC :: Cpu c => CSM3 c (Map Reference (Maybe Reference, Either (InfInt64, InfInt64) Int64))
getPositionsC = state (\s -> (cs3Position s, s))

addInlineC :: Cpu c => Reference -> (Int64, Maybe (Expr4 c)) -> CSM3 c ()
addInlineC n v = tell mempty{cs3Inline = M.singleton n v}

resolveNameC :: Cpu c => [(Location, String)] -> Location -> Text -> Reference -> CSM3 c Reference
resolveNameC errs loc name par = state go
  where
    go s@CSt3{..} = case go' of
      [] -> printErrorS s $ (loc, "1/Can't find name \"" ++ unpack name ++ "\" on path " ++ show par):errs++[sourcePos||]
      [path] -> (path, s)
      _ -> printErrorS s $ (loc, "Name \"" ++ unpack name ++ "\" is not unique on path " ++ show par):errs++[sourcePos||]
      where
        go' = maybeToList $ R.lookup par name cs3Data

getCallPathsForReferenceC :: Cpu c => Reference -> CSM3 c (Set [Text])
getCallPathsForReferenceC n = state go
  where
    go s@CSt3{..} =
      let
        n' = initEx $ R.pathOfReference n
        s' = mapMaybe (\(k, v) -> bool Nothing (Just v) (isPrefixOf n' k)) $ M.toList cs3CallPaths
      in
        (concat (S.singleton n':s'), s)
