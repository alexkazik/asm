{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Asm.Core.PrettyPrint.Pretty
  ( Pretty (..)
  , showPretty
  ) where

import           Asm.Core.Prelude
import qualified Data.Map.Strict                  as M
import qualified Data.Set                         as S
import qualified Data.Text.Prettyprint.Doc        as PP
import           Data.Void                        (Void)
import qualified Language.Haskell.TH              as TH

import           Asm.Core.Data.ByteVal
import           Asm.Core.PrettyPrint.PrettyPrint
import           Asm.Core.PrettyPrint.PrettySrc
import           Asm.Data.BitList
import           Asm.Data.InfInt64

class Pretty a where
  pretty :: a -> Doc
  default pretty :: PP.Pretty a => a -> Doc
  pretty = PP.pretty

  prettyList :: [a] -> Doc
  prettyList = list . map pretty

-- instances by default

instance Pretty Bool
instance Pretty Char
instance Pretty Double
instance Pretty Float
instance Pretty Int
instance Pretty Int64
instance Pretty Integer
instance Pretty LText
instance Pretty String
instance Pretty Text
instance Pretty [Text]
instance Pretty Void
instance Pretty Word
instance Pretty Word16
instance Pretty Word8
instance Pretty ()

-- instance by show

instance Pretty InfInt64 where
  pretty = pshow

instance Pretty TH.Info where
  pretty = pshow

instance Pretty TH.Name where
  pretty = pshow

instance Pretty TH.Type where
  pretty = pshow

-- more complex instances

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  pretty (a, b, c) = tupled [pretty a, pretty b, pretty c]

instance (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (a, b, c, d) where
  pretty (a, b, c, d) = tupled [pretty a, pretty b, pretty c, pretty d]

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing  = mempty
  pretty (Just x) = pretty x

instance Pretty a => Pretty (Ratio a) where
  pretty r = pretty (numerator r) ++ pretty '%' ++ pretty (denominator r)

instance (Pretty l, Pretty r) => Pretty (Either l r) where
  pretty (Left l)  = "L:" <+> pretty l
  pretty (Right r) = "R:" <+> pretty r

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty m = align $ vsep (map elem' $ M.toList m)
    where
      elem' (k, v) = pretty k ++ pretty ':' <+> pretty v

instance (Pretty v) => Pretty (S.Set v) where
  pretty m = align $ vsep (map pretty $ S.toList m)

-- instance for ByteVal
instance PrettySrc c => Pretty (ByteVal c) where
  pretty ByteValAny   = "x{0..255}"
  pretty (ByteValConst x) = case checkPopulation x of
    PrNone        -> error "Asm.Data.ByteValSimple.show invariant: no bits set"
    (PrSingle x') -> "c{" ++ pretty x' ++ "}"
    PrMany        -> "c{" ++ fillListNoSpace (map pretty $ Asm.Data.BitList.toList x) ++ "}"
    PrAll         -> "c{0..255}"
  pretty (ByteValInit x) = case checkPopulation x of
    PrNone        -> error "Asm.Data.ByteValSimple.show invariant: no bits set"
    (PrSingle x') -> "i{" ++ pretty x' ++ "}"
    PrMany        -> "i{" ++ fillListNoSpace (map pretty $ Asm.Data.BitList.toList x) ++ "}"
    PrAll         -> "i{0..255}"
  pretty (ByteValCode ByteValIsConst x) = "c{" ++ prettySrc x ++ "}"
  pretty (ByteValCode ByteValIsInit x) = "i{" ++ prettySrc x ++ "}"
  pretty (ByteValLocal x) = "local{" ++ pretty x ++ "}"

-- convert Pretty to String
showPretty :: Pretty p => p -> String
showPretty x = show $ pretty x
