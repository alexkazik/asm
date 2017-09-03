{-# LANGUAGE FlexibleInstances #-}

module Asm.Core.Data.Tree
  ( Reference
  , Tree
  -- Reference
  , root
  -- Tree
  , empty
  , lookup
  , search
  , parent
  , insert
  , insertUnlinked
  , link
  , get
  , set
  , nameOfReference
  , pathOfReference
  ) where

import           Asm.Core.Prelude     hiding (empty, get, lookup)
import qualified Data.Map.Strict      as M
import qualified Data.Vector          as V

import           Asm.Core.PrettyPrint

-- Types

data Entry a =
  Entry
  { eParent :: !(Maybe Reference)
  , eKind   :: !a
  , eDir    :: M.Map Text Reference
  }

newtype Tree a = Tree (Vector (Entry a))

instance Pretty a => Pretty (Tree a) where
  pretty (Tree tree) = vsep (map dumpName $ V.toList $ V.indexed tree) <|+|> vsep (map dumpDir $ V.toList $ V.indexed tree)
    where
      dumpName (i, e) = fillBreak 4 (pretty i) <+> pretty '=' <+> pretty (eKind e)
      dumpDir (i, e) = fillBreak 4 (pretty i) <+> pretty '=' <+> pretty (eDir e)

newtype Reference = Reference (Int, [Text])

instance Eq Reference where
  {-# INLINE (==) #-}
  (Reference (a, _)) == (Reference (b, _)) = a == b

instance Ord Reference where
  {-# INLINE compare #-}
  (Reference (a, _)) `compare` (Reference (b, _)) = a `compare` b

instance Show Reference where
  {-# INLINABLE show #-}
  show (Reference (i, p)) = show i ++ "#" ++ unpack (intercalate "." p)

instance Pretty Reference where
  {-# INLINABLE pretty #-}
  pretty (Reference (i, p)) = pretty (tshow i ++ "#" ++ intercalate "." p)

instance Pretty [Reference] where
  pretty = prettyList

-- Reference

root :: Reference
root = Reference (0, [])

-- Tree

empty :: a -> Tree a
empty k = Tree $ V.singleton (Entry Nothing k M.empty)

lookup :: Reference -> Text -> Tree a -> Maybe Reference
lookup (Reference (i, _)) e (Tree d) = M.lookup e (eDir $ d V.! i)

search :: Reference -> Text -> Tree a -> [Reference]
search p e d = ordNub $ catMaybes (go (Just p))
  where
    go :: Maybe Reference -> [Maybe Reference]
    go (Just p') = lookup p' e d : go (parent p' d)
    go Nothing   = []

parent :: Reference -> Tree a -> Maybe Reference
parent (Reference (i, _)) (Tree d) = eParent (d V.! i)

insert :: Reference -> Text -> a -> Tree a -> Either Reference (Reference, Tree a)
insert p@(Reference (i, q)) e k (Tree d) =
  let
    i' = V.length d
    p' = Reference (i', q ++ [e])
    entry = d V.! i
    dir = eDir entry
    dir' = M.insert e p' dir
    entry' = entry{eDir = dir'}
  in
    case M.lookup e dir of
      Just p'' -> Left p''
      Nothing  -> Right (p', Tree $ (d V.// [(i, entry')]) `V.snoc` Entry (Just p) k M.empty)

insertUnlinked :: Reference -> Text -> a -> Tree a -> (Reference, Tree a)
insertUnlinked p@(Reference (_, q)) e k (Tree d) =
  let
    i' = V.length d
    p' = Reference (i', q ++ [e])
  in
    (p', Tree $ d `V.snoc` Entry (Just p) k M.empty)

link :: Reference -> Text -> Reference -> Tree a -> Either Reference (Tree a)
link (Reference (i, _)) e l d'@(Tree d) =
  let
    entry = d V.! i
    dir = eDir entry
    dir' = M.insert e l dir
    entry' = entry{eDir = dir'}
  in
    case M.lookup e dir of
      Just l' -> bool (Left l') (Right d') (l' == l)
      Nothing -> Right (Tree $ d V.// [(i, entry')])

get :: Reference -> Tree a -> a
get (Reference (i, _)) (Tree d) = eKind (d V.! i)

set :: Reference -> a -> Tree a -> Tree a
set (Reference (i, _)) k (Tree d) = Tree $ d V.// [(i, (d V.! i){eKind = k})]

nameOfReference :: Reference -> Text
nameOfReference (Reference (_, p)) = intercalate "." p

pathOfReference :: Reference -> [Text]
pathOfReference (Reference (_, p)) = p
