module Asm.Core.Phase3.CallPath
  ( convertCallPathC
  , mergeCallPaths
  ) where

import           Asm.Core.Prelude
import qualified Data.List                      as L
import qualified Data.Map                       as M
import qualified Data.Set                       as S

import           Asm.Core.Data.Cpu
import           Asm.Core.Data.Tree
import           Asm.Core.Data.TypeDefinition
import           Asm.Core.Phase3.CompilerState3
import           Asm.Core.Phase3.Data.Expr3
import           Asm.Core.Phase3.MetaDataApply
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.Phases34.EvaluateExpr

convertCallPathC :: Cpu c => ([Text], [Expr3 c]) -> CSM3 c ()
convertCallPathC (cwd, jt) = do
  jt'x <- mapM applyMetaExprC jt
  jt' <- mapM evaluateExprC jt'x
  let
    jt'' =
      filter (\p -> not (L.null p || L.isPrefixOf cwd p)) $
      map (dropLastMaybeTrailingSystem . pathOfReference) $
      mapMaybe toPath jt'
  if null jt''
    then return ()
    else state (\s -> ((), s{cs3CallPaths = M.insert cwd (S.fromList jt'') (cs3CallPaths s)}))
  where
    toPath (_, E4Pointer _ l TDCode _) = Just l
    toPath _                           = Nothing
    dropLastMaybeTrailingSystem :: [Text] -> [Text]
    dropLastMaybeTrailingSystem x =
      let
        Just (pth, lab) = unsnoc x
      in
        if isPrefixOf "_" lab
          then pth
          else L.dropWhileEnd (isPrefixOf "~") pth

mergeCallPaths :: Map [Text] (Set [Text]) -> Map [Text] (Set [Text])
mergeCallPaths acp =
  let
    acp' = M.mapWithKey (\k _ -> fst (go (S.empty, []) k)) acp
  in
    map (\x -> S.filter (\y -> not (any (\z -> z `isPrefixOf` y && y /= z) x)) x) acp'
  where
    go :: (Set [Text], [[Text]]) -> [Text] -> (Set [Text], [[Text]])
    go (res, seen) k =
      if k `elem` seen
        then (res, seen)
        else
          let
            seen' = k:seen
            todo = concatMap (\(k', v') -> k':(S.toList v')) $ filter (\(k',_) -> k `isPrefixOf` k') $ M.toList acp
            res' = res <> S.fromList todo
          in
            foldl' go (res', seen') todo
