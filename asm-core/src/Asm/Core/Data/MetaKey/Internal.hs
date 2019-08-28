{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Asm.Core.Data.MetaKey.Internal
  ( MetaKey
  , mkCompilerMetaKeys
  , mkCpuMetaKeys
  , MetaKeyMap
  , mkmEmpty
  , mkmInsert
  , mkmDelete
  , mkmLookup
  , MetaKeySet
  , mksEmpty
  , mksInsert
  , mksDelete
  , mksMember
  , mksUnion
  , mksToList
  ) where

import           Asm.Core.Prelude
import qualified Data.IntMap         as IM
import qualified Data.IntSet         as IS
import qualified Language.Haskell.TH as TH


newtype MetaKey = MetaKey Int deriving (Eq)

instance Show MetaKey where
  show (MetaKey mk) = '%' : show mk


mkCompilerMetaKeys :: [String] -> TH.Q [TH.Dec]
mkCompilerMetaKeys = mkMetaKeys . zip [0 .. maxBound]

mkCpuMetaKeys :: [String] -> TH.Q [TH.Dec]
mkCpuMetaKeys = mkMetaKeys . zip [-1, -2 .. minBound]

mkMetaKeys :: [(Int, String)] -> TH.Q [TH.Dec]
mkMetaKeys = return . concatMap go
  where
    go (mk, name) =
      let
        body = TH.AppE (TH.ConE 'MetaKey) (TH.LitE $ TH.IntegerL $ fromIntegral mk)
      in
        [ TH.SigD (TH.mkName name) (TH.ConT ''MetaKey)
        , TH.FunD (TH.mkName name) [TH.Clause [] (TH.NormalB body) []]
        ]


newtype MetaKeyMap a = MetaKeyMap (IM.IntMap a)

mkmEmpty :: MetaKeyMap a
mkmEmpty = MetaKeyMap IM.empty

mkmInsert :: MetaKey -> a -> MetaKeyMap a -> MetaKeyMap a
mkmInsert (MetaKey k) v (MetaKeyMap m) = MetaKeyMap (IM.insert k v m)

mkmDelete :: MetaKey -> MetaKeyMap a -> MetaKeyMap a
mkmDelete (MetaKey k) (MetaKeyMap m) = MetaKeyMap (IM.delete k m)

mkmLookup :: MetaKey -> MetaKeyMap a -> Maybe a
mkmLookup (MetaKey k) (MetaKeyMap m) = IM.lookup k m


newtype MetaKeySet = MetaKeySet IS.IntSet

mksEmpty :: MetaKeySet
mksEmpty = MetaKeySet IS.empty

mksInsert :: MetaKey -> MetaKeySet -> MetaKeySet
mksInsert (MetaKey k) (MetaKeySet m) = MetaKeySet (IS.insert k m)

mksDelete :: MetaKey -> MetaKeySet -> MetaKeySet
mksDelete (MetaKey k) (MetaKeySet m) = MetaKeySet (IS.delete k m)

mksMember :: MetaKey -> MetaKeySet -> Bool
mksMember (MetaKey k) (MetaKeySet m) = IS.member k m

mksUnion :: MetaKeySet -> MetaKeySet -> MetaKeySet
mksUnion (MetaKeySet a) (MetaKeySet b) = MetaKeySet (IS.union a b)

mksToList :: MetaKeySet -> [MetaKey]
mksToList (MetaKeySet m) = map MetaKey (IS.toList m)
