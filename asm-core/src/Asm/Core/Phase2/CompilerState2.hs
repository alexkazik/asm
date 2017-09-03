module Asm.Core.Phase2.CompilerState2
  ( module Asm.Core.Phase2.Data.CompilerState2
  , module Asm.Core.Phase2.CompilerState2
  , module Asm.Core.Phases.Data.CompilerState1234
  , module Asm.Core.Phases12.Data.CompilerState12
  ) where

import           Asm.Core.Prelude
import qualified Data.Map.Strict                        as M
import           Data.Proxy

import           Asm.Core.Data.Cpu
import           Asm.Core.Data.FunctionKey
import           Asm.Core.Data.KindDefinition
import           Asm.Core.Data.Reference
import qualified Asm.Core.Data.Tree                     as R
import           Asm.Core.Phase1.Data.CompilerState1
import           Asm.Core.Phase1.Data.Expr12
import           Asm.Core.Phase2.Data.CompilerState2
import           Asm.Core.Phase3.Data.Expr3
import           Asm.Core.Phases.Data.CompilerState1234
import           Asm.Core.Phases12.Data.CompilerState12
import           Asm.Core.SourcePos

initialState2 :: forall c. Cpu c => CompilerState1 c -> CompilerState2 c
initialState2 CSt1{..} =
  CSt2
    { cs2Path = R.root
    , cs2AliasPath = [R.root]
    , cs2Data = cs1Data
    , cs2PoolDefinition = cs1PoolDefinition
    , cs2Aliases = cs1Aliases
    , cs2TypeInExpr = M.empty
    , cs2Position = M.empty
    , cs2FunctionMap = foldr fklmInsert fklmEmpty (compilerFunctionKeys ++ cpuFunctionKeys (Proxy :: Proxy c))
    , cs2CallPaths = M.empty
  }

getAliasC :: Cpu c => Reference -> CSM2 c (Maybe (Expr12 c))
getAliasC k = state (\s -> (M.lookup k (cs2Aliases s), s))

addTypeInExprC :: Cpu c => Reference -> Expr3 c -> CSM2 c ()
addTypeInExprC k v = state (\s -> ((), s{cs2TypeInExpr = M.insert k v (cs2TypeInExpr s)}))

resolveNameC :: Cpu c => [(Location, String)] -> Location -> Text -> CSM2 c Reference
resolveNameC errs loc name = state go
  where
    go s@CSt2{..} = case go' of
      [] -> printErrorS s $ (loc, "Can't find name \"" ++ unpack name ++ "\" on path " ++ show cs2Path):errs++[sourcePos||]
      [path] -> (path, s)
      ps -> printErrorS s $ (loc, "1/Name \"" ++ unpack name ++ "\" is not unique on path " ++ show cs2Path):([], show ps):errs++[sourcePos||]
      where
        go'
          | isPrefixOf "__" name = maybeToList $ R.lookup cs2Path name cs2Data
          | otherwise = R.search cs2Path name cs2Data

getKindC :: Cpu c => Reference -> CSM2 c (Location, KindDefinition)
getKindC i = state (\s -> (R.get i (cs2Data s), s))

setKindC :: Cpu c => Reference -> (Location, KindDefinition) -> CSM2 c ()
setKindC i t = state (\s -> ((), s{cs2Data = R.set i t (cs2Data s)}))

getTypeInExprC :: Cpu c => Reference -> CSM2 c (Expr3 c)
getTypeInExprC k = state (\s -> (cs2TypeInExpr s M.! k, s))

setPositionC :: Cpu c => Reference -> CSM2 c ()
setPositionC n = state (\s -> ((), s{cs2Position = M.insert n (Nothing, Left (minBound, maxBound)) (cs2Position s)}))

lookupFunctionName :: Cpu c => Text -> CSM2 c (Maybe FunctionKey)
lookupFunctionName n = state (\s -> (fklmLookup n (cs2FunctionMap s), s))

addCallPathsC :: [Text] -> [Expr3 c] -> CSM2 c ()
addCallPathsC k v = state (\s -> ((), s{cs2CallPaths = M.insertWith (<>) k v (cs2CallPaths s)}))
