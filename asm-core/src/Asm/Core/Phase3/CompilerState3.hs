{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Asm.Core.Phase3.CompilerState3
  ( module Asm.Core.Phase3.Data.CompilerState3
  , module Asm.Core.Phases34.Data.CompilerState34
  , module Asm.Core.Phases.Data.CompilerState1234
  , initialReader3
  , initialState3
  , addNameC
  , getTypeInExprC
  , getKindC
  , getPositionsC
  , addInlineC
  , resolveNameC
  , getCallPathsForReferenceC
  ) where

import           Asm.Core.Prelude
import qualified Data.Map.Strict                        as M
import qualified Data.Set                               as S

import           Asm.Core.Control.CompilerError
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
    ep :: (PoolData c)
    ep = PoolDataStartFlat{stateStart=[], stateFlat=[], stateData=S.empty, statePool=[]}

createPools :: a -> Map Reference a -> PoolDefinition -> Map Reference a
createPools def pools pd = foldl' (createPool def) pools (pdPools pd)
createPool :: a -> Map Reference a -> Reference -> Map Reference a
createPool def pools na = M.insert na def pools


addNameC :: Cpu c => Text -> CSM3 c Reference
addNameC name = do
  s@CSt3{..} <- get
  let
    (path, tree) = R.insertUnlinked R.root name ([], KDCpu) cs3Data
  put s{cs3Data = tree}
  return path

getTypeInExprC :: Cpu c => Reference -> CSM3 c (Expr3 c)
getTypeInExprC k = asks (\s -> cs3TypeInExpr s M.! k)


getKindC :: Cpu c => Reference -> CSM3 c (Location, KindDefinition)
getKindC i = gets (\CSt3{..} -> R.get i cs3Data)

getPositionsC :: Cpu c => CSM3 c (Map Reference (Maybe Reference, Either (InfInt64, InfInt64) Int64))
getPositionsC = gets cs3Position

addInlineC :: Cpu c => Reference -> (Int64, Maybe (Expr4 c)) -> CSM3 c ()
addInlineC n v = tell mempty{cs3Inline = M.singleton n v}

resolveNameC :: Cpu c => [(Location, String)] -> Location -> Text -> Reference -> CSM3 c Reference
resolveNameC errs loc name par =
  gets go >>= \case
      [] -> $throwFatalError ((loc, "1/Can't find name \"" ++ unpack name ++ "\" on path " ++ show par):errs)
      [path] -> return path
      _ -> $throwFatalError ((loc, "Name \"" ++ unpack name ++ "\" is not unique on path " ++ show par):errs)
  where
    go CSt3{..} = maybeToList $ R.lookup par name cs3Data

getCallPathsForReferenceC :: Cpu c => Reference -> CSM3 c (Set [Text])
getCallPathsForReferenceC n = gets go
  where
    go CSt3{..} =
      let
        n' = initEx $ R.pathOfReference n
        s' = mapMaybe (\(k, v) -> bool Nothing (Just v) (n' `isPrefixOf` k)) $ M.toList cs3CallPaths
      in
        concat (S.singleton n':s')
