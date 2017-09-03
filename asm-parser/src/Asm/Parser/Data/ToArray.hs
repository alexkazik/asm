{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Asm.Parser.Data.ToArray
  ( ToArray(..)
  ) where

import           Asm.Core.Prelude
import qualified Data.Foldable
import qualified Data.Vector             as V
import qualified Data.Vector.Storable    as SV

import           Asm.Core.SourcePos
import           Asm.Data.ByteValSimple

import           Asm.Parser.Data.PExpr
import           Asm.Parser.Data.ToExpr
import           Asm.Parser.Parser.Tools


class ToArray pe a where
  toArray :: CpuParser c ps pe => SourcePos -> a -> PExpr pe

instance {-# OVERLAPPING #-} ToArray pe (Vector Word8) where
  toArray loc v = (loc, PEUserArrayBVS $ SV.map byteValSimpleWord8 $ V.convert v)

instance {-# OVERLAPPING #-} ToArray pe (Vector ByteValSimple) where
  toArray loc v = (loc, PEUserArrayBVS $ V.convert v)

instance {-# OVERLAPPING #-} ToArray pe (SV.Vector Word8) where
  toArray loc v = (loc, PEUserArrayBVS $ SV.map byteValSimpleWord8 v)

instance {-# OVERLAPPING #-} ToArray pe (SV.Vector ByteValSimple) where
  toArray loc v = (loc, PEUserArrayBVS v)

instance {-# OVERLAPPING #-} Foldable f => ToArray pe (f Word8) where
  toArray loc v = (loc, PEUserArrayBVS $ SV.fromList $ map byteValSimpleWord8 $ Data.Foldable.toList v)

instance {-# OVERLAPPING #-} Foldable f => ToArray pe (f ByteValSimple) where
  toArray loc v = (loc, PEUserArrayBVS $ SV.fromList $ Data.Foldable.toList v)

instance {-# OVERLAPPABLE #-} (ToExpr pe a, Foldable f) => ToArray pe (f a) where
  toArray loc v = (loc, PEUserArray $ map (toExpr loc) $ V.fromList $ Data.Foldable.toList v)
