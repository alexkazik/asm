module Asm.Core.Data.FunctionKey.Internal
  ( FunctionKey
  , functionKeyName
  , mkCompilerFunctionKeys
  , mkCpuFunctionKeys
  , FunctionKeyLookupMap
  , fklmEmpty
  , fklmInsert
  , fklmLookup
  , FunctionKeyMap
  , fkmEmpty
  , fkmInsert
  , fkmInsertWith
  , fkmLookup
  ) where

import           Asm.Core.Prelude
import qualified Data.IntMap          as IM
import qualified Data.Map             as M
import qualified Language.Haskell.TH  as TH

import           Asm.Core.PrettyPrint
import           Asm.Core.SourcePos

newtype FunctionKey = FunctionKey (Int, Text)
  deriving (Data, Typeable)

functionKeyName :: FunctionKey -> Text
functionKeyName (FunctionKey (_, f)) = f

instance Show FunctionKey where
  show (FunctionKey (i, n)) = '%' : show i ++ ':' : show n

instance Eq FunctionKey where
  (FunctionKey (l, _)) == (FunctionKey (r, _)) = l == r

instance Pretty FunctionKey where
  pretty = pshow

mkCompilerFunctionKeys :: String -> [(String, String)] -> TH.Q [TH.Dec]
mkCompilerFunctionKeys n = mkFunctionKeys n . zip [0 .. maxBound]

mkCpuFunctionKeys :: String -> [(String, String)] -> TH.Q [TH.Dec]
mkCpuFunctionKeys n = mkFunctionKeys n . zip [-1, -2 .. minBound]

mkFunctionKeys :: String -> [(Int, (String, String))] -> TH.Q [TH.Dec]
mkFunctionKeys outName fns = do
  let
    outName' = TH.mkName outName
    fns' = map go fns
  return $
    concatMap fst fns' ++
    [ TH.SigD outName' (TH.AppT TH.ListT (TH.ConT ''FunctionKey))
    , TH.FunD outName' [TH.Clause [] (TH.NormalB $ TH.ListE $ map snd fns') []]
    ]
  where
    go :: (Int, (String, String)) -> ([TH.Dec], TH.Exp)
    go (mk, (name, fn)) =
      let
        name' = TH.mkName name
        body = TH.AppE (TH.ConE 'FunctionKey) (TH.TupE [ TH.LitE $ TH.IntegerL $ fromIntegral mk , TH.LitE $ TH.StringL fn] )
      in
        ( [ TH.SigD name' (TH.ConT ''FunctionKey)
          , TH.FunD name' [TH.Clause [] (TH.NormalB body) []]
          ]
        , TH.VarE name'
        )

newtype FunctionKeyLookupMap = FunctionKeyLookupMap (M.Map Text Int)

fklmEmpty :: FunctionKeyLookupMap
fklmEmpty = FunctionKeyLookupMap M.empty

fklmInsert :: FunctionKey -> FunctionKeyLookupMap -> FunctionKeyLookupMap
fklmInsert (FunctionKey (i, n)) (FunctionKeyLookupMap m) = FunctionKeyLookupMap (M.insertWith (printError [sourcePos|Duplicate FunctionKey|]) (toLower n) i m)

fklmLookup :: Text -> FunctionKeyLookupMap -> Maybe FunctionKey
fklmLookup n (FunctionKeyLookupMap m) = map (\i -> FunctionKey (i, n)) (M.lookup (toLower n) m)

newtype FunctionKeyMap a = FunctionKeyMap (IM.IntMap a)

fkmEmpty :: FunctionKeyMap a
fkmEmpty = FunctionKeyMap IM.empty

fkmInsert :: FunctionKey -> a -> FunctionKeyMap a -> FunctionKeyMap a
fkmInsert (FunctionKey (i, _)) v (FunctionKeyMap m) = FunctionKeyMap (IM.insertWith (printError [sourcePos|Duplicate FunctionKey|]) i v m)

fkmInsertWith :: (a -> a -> a) -> FunctionKey -> a -> FunctionKeyMap a -> FunctionKeyMap a
fkmInsertWith f (FunctionKey (i, _)) v (FunctionKeyMap m) = FunctionKeyMap (IM.insertWith f i v m)

fkmLookup :: FunctionKey -> FunctionKeyMap a -> Maybe a
fkmLookup (FunctionKey (i, _)) (FunctionKeyMap m) = IM.lookup i m
