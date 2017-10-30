module Asm.Core.Phase1.CompilerState1
  ( module Asm.Core.Phase1.Data.CompilerState1
  , module Asm.Core.Phase1.CompilerState1
  , module Asm.Core.Phases.Data.CompilerState1234
  , module Asm.Core.Phases12.Data.CompilerState12
  ) where

import           Asm.Core.Prelude
import qualified Data.Map.Strict                        as M
import           Data.Proxy

import           Asm.Core.Data.Cpu
import           Asm.Core.Data.KindDefinition
import           Asm.Core.Data.MetaKey
import           Asm.Core.Data.Reference
import qualified Asm.Core.Data.Tree                     as R
import           Asm.Core.Data.TypeDefinition
import           Asm.Core.Phase1.Data.CompilerState1
import           Asm.Core.Phase1.Data.Expr12
import           Asm.Core.Phase1.Data.InitTree
import           Asm.Core.Phases.Data.CompilerState1234
import           Asm.Core.Phases.Data.PoolDefinition
import           Asm.Core.Phases12.Data.CompilerState12
import           Asm.Core.SourcePos

initialState1 :: forall c. Cpu c => CompilerState1 c
initialState1 =
  CSt1
    { cs1Path = R.root
    , cs1AliasPath = [R.root]
    , cs1Data = initTree R.root initialData (R.empty (spBuiltin, KDNamespace))
    , cs1UniqueNumber = 0
    , cs1OnlySuperLocals = 0
    , cs1OnlySystemNames = 0
    }
  where
    initialData =
      [ I ("byte", KDType TDByte, [])
      , I ("code", KDType TDCode, [])
      , I ("pool", KDType poolType, [])
      , I ("meta", KDNamespace,
          [ I ("pool", KDMeta metaPool,
              [ I ("code", KDMeta metaPoolCode, [])
              , I ("const", KDMeta metaPoolConst, [])
              , I ("local", KDMeta metaPoolLocal, [])
              , I ("var", KDMeta metaPoolVar, [])
              ]
            )
          , I ("check", KDMeta metaCheck,
              [ I ("data", KDMeta metaCheckData, [])
              , I ("data8", KDMeta metaCheckData8, [])
              , I ("fill", KDMeta metaCheckFill, [])
              , I ("fill8", KDMeta metaCheckFill8, [])
              ]
            )
          ]
        )
      ] ++ cpuGetInitTree (Proxy :: Proxy c)
    initTree :: Reference -> [InitTree] -> R.Tree (Location, KindDefinition) -> R.Tree (Location, KindDefinition)
    initTree _ [] t = t
    initTree r (I (e, k, l):is) t =
      case insert' r e k t of
        Right (r', t') -> initTree r is (initTree r' l t')
        -- Left r'        -> printError (([], "unable to create initial data: " ++ show (r, e, r')):[sourcePos|unable to create initial data|])
        _              -> printError [sourcePos|unable to create initial data|]
    insert' :: Reference -> Text -> KindDefinition -> R.Tree (Location, KindDefinition) -> Either Reference (Reference, R.Tree (Location, KindDefinition))
    insert' p e k d =
      either check Right $ R.insert p e (spBuiltin, k) d
      where
        check p' = check' p' (R.get p' d)
        check' p' (_, k')
          | k == k' || k == KDNamespace = Right (p', d)
          | otherwise = Left p'

addAliasC :: Cpu c => Reference -> Expr12 c -> CSM1 c ()
addAliasC k v = tell mempty{cs1Aliases = M.singleton k v}

addNameC :: Cpu c => Text -> (Location, KindDefinition) -> CSM1 c Reference
addNameC name type'@(loc,_) = state go
  where
    go s@CSt1{..} =
      case R.insert cs1Path name type' cs1Data of
        Left _ref -> printErrorS s ((loc, "duplicate/1"):[sourcePos||])
        Right (path, tree) ->
          if isPrefixOf "~" name || cs1OnlySystemNames == 0
            then
              if isPrefixOf "__" name
                then (path, s{cs1Data = tree})
                else case R.link (headEx cs1AliasPath) name path tree of
                  Left ref    -> printErrorS s $ (loc, "duplicate/2 name " ++ unpack name):([],"path: " ++ show cs1AliasPath):([],"dest: " ++ show ref):[sourcePos||]
                  Right tree' -> (path, s{cs1Data = tree'})
            else printErrorS s ((loc, "no names allowed"):[sourcePos||])

getUniqueNameC :: Cpu c => CSM1 c Text
getUniqueNameC = do
  s@CSt1{..} <- get
  put s{cs1UniqueNumber = cs1UniqueNumber + 1}
  return ('~' `cons` tshow cs1UniqueNumber)

pushSuperLocalsModeC :: Cpu c => CSM1 c ()
pushSuperLocalsModeC = modify (\s -> s{cs1OnlySuperLocals= cs1OnlySuperLocals s + 1})

popSuperLocalsModeC :: Cpu c => CSM1 c ()
popSuperLocalsModeC = modify (\s -> s{cs1OnlySuperLocals = cs1OnlySuperLocals s - 1})

pushSystemNameModeC :: Cpu c => CSM1 c ()
pushSystemNameModeC = modify (\s -> s{cs1OnlySystemNames= cs1OnlySystemNames s + 1})

popSystemNameModeC :: Cpu c => CSM1 c ()
popSystemNameModeC = modify (\s -> s{cs1OnlySystemNames = cs1OnlySystemNames s - 1})

setKindC :: Cpu c => Reference -> (Location, KindDefinition) -> CSM1 c ()
setKindC i t = modify (\s -> s{cs1Data = R.set i t (cs1Data s)})

addPoolC :: Cpu c => Location -> Text -> [Text] -> Bool -> Int64 -> Int64 -> CSM1 c ()
addPoolC l n' u' v st b = do
  -- add name and remember the absolute one
  idName <- addNameC n' (l, KDPointer (TDPool True False v))
  -- add child names and remember the absolute ones
  u <- mapM (`addNameC` (l, KDPointer (TDPool False True v))) u'
  -- add the other pool stuff
  tell mempty{cs1PoolDefinition = M.singleton idName PoolDefinition{pdBank = b, pdStart = st, pdVirtual = v, pdPools = u}}

addPoolBothC :: Cpu c => Location -> Text -> Bool -> Int64 -> Int64 -> CSM1 c ()
addPoolBothC l n' v st b = do
  -- add name and remember the absolute one
  idName <- addNameC n' (l, KDPointer (TDPool True True v))
  -- add the other pool stuff
  tell mempty{cs1PoolDefinition = M.singleton idName PoolDefinition{pdBank = b, pdStart = st, pdVirtual = v, pdPools = [idName]}}
