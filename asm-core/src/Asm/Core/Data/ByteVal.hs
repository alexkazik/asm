module Asm.Core.Data.ByteVal
  ( module Asm.Data.Word256
  , ByteVal(..)
  , ConstOrInit(..)
  , byteValSetConstOrInit
  , byteValWord8
  , byteValMaskedWord8
  , toByteValSimple
  , fromByteValSimple
  ) where

import           Asm.Core.Prelude
import qualified Data.Foldable

import           Asm.Data.BitList
import           Asm.Data.ByteValSimple
import           Asm.Data.Word256

data ConstOrInit
  = ByteValIsConst
  | ByteValIsInit
  deriving (Eq, Ord, Show, Data, Typeable)

data ByteVal c
  = ByteValAny
  | ByteValConst !Word256 -- invariant: value can't be zero
  | ByteValInit !Word256 -- invariant: value can't be zero
  | ByteValCode !ConstOrInit !c
  | ByteValLocal (Set [Text]) -- invariant: set can't be empty
  deriving (Data, Typeable)

instance Eq c => Eq (ByteVal c) where
  ByteValAny == ByteValAny = True
  (ByteValConst a) == (ByteValConst b) = a == b
  (ByteValInit a) == (ByteValInit b) = a == b
  (ByteValCode a c) == (ByteValCode b d) = a == b && c == d
  (ByteValLocal a) == (ByteValLocal b) = a == b
  _ == _ = False

instance Show c => Show (ByteVal c) where
  show ByteValAny = "x{0..255}"
  show (ByteValConst x) = case checkPopulation x of
    PrNone        -> error "Asm.Data.ByteValSimple.show invariant: no bits set"
    (PrSingle x') -> "c{" ++ show x' ++ "}"
    PrMany        -> "c{" ++ show (ByteValSimple x) ++ "}"
    PrAll         -> "c{0..255}"
  show (ByteValInit x) = case checkPopulation x of
    PrNone        -> error "Asm.Data.ByteValSimple.show invariant: no bits set"
    (PrSingle x') -> "i{" ++ show x' ++ "}"
    PrMany        -> "i{" ++ show (ByteValSimple x) ++ "}"
    PrAll         -> "i{0..255}"
  show (ByteValCode ByteValIsConst c) = "c{@" ++ show c ++ "}"
  show (ByteValCode ByteValIsInit c) = "i{@" ++ show c ++ "}"
  show (ByteValLocal c) = "local{@" ++ show c ++ "}"

byteValSetConstOrInit :: ConstOrInit -> ByteVal c -> ByteVal c
byteValSetConstOrInit _              ByteValAny        = ByteValAny
byteValSetConstOrInit ByteValIsConst x@ByteValConst{}  = x
byteValSetConstOrInit ByteValIsInit  x@ByteValInit{}   = x
byteValSetConstOrInit ByteValIsConst (ByteValInit x)   = ByteValConst x
byteValSetConstOrInit ByteValIsInit  (ByteValConst x)  = ByteValInit x
byteValSetConstOrInit constOrInit    (ByteValCode _ c) = ByteValCode constOrInit c
byteValSetConstOrInit _              ByteValLocal{}    = error "Asm.Data.ByteValSimple.byteValSetConstOrInit: can't change local"

byteValWord8 :: ConstOrInit -> Word8 -> ByteVal c
byteValWord8 ByteValIsConst x = ByteValConst (bit $ fromIntegral x)
byteValWord8 ByteValIsInit x  = ByteValInit (bit $ fromIntegral x)

byteValMaskedWord8 :: ConstOrInit -> Word8 -> Word8 -> ByteVal c
byteValMaskedWord8 constOrInit v m =
  let
    mv = Asm.Data.BitList.fromList $ filter (\x -> (fromIntegral x .&. m) == v .&. m) [0 .. 255]
  in
    case constOrInit of
      ByteValIsConst -> ByteValConst mv
      ByteValIsInit  -> ByteValInit mv

fromByteValSimple :: ConstOrInit -> ByteValSimple -> ByteVal c
fromByteValSimple ByteValIsConst (ByteValSimple x) = ByteValConst x
fromByteValSimple ByteValIsInit (ByteValSimple x)  = ByteValInit x

toByteValSimple :: Show c => ByteVal c -> ByteValSimple
toByteValSimple ByteValAny        = byteValSimpleAny
toByteValSimple (ByteValConst x)  = ByteValSimple x
toByteValSimple (ByteValInit x)   = ByteValSimple x
toByteValSimple ByteValLocal{}    = ByteValSimple maxBound
toByteValSimple (ByteValCode _ c) = error $ "Asm.Core.Data.ByteVal.toByteValSimple can't use ByteValCode: " ++ show c

instance Foldable ByteVal where
  foldl f z (ByteValCode _ x) = f z x
  foldl _ z _                 = z
  foldr f z (ByteValCode _ x) = f x z
  foldr _ z _                 = z

instance Functor ByteVal where
  fmap _ ByteValAny         = ByteValAny
  fmap _ (ByteValConst x)   = ByteValConst x
  fmap _ (ByteValInit x)    = ByteValInit x
  fmap f (ByteValCode ci x) = ByteValCode ci (f x)
  fmap _ (ByteValLocal x)   = ByteValLocal x

instance Traversable ByteVal where
  traverse _ ByteValAny         = pure ByteValAny
  traverse _ (ByteValConst x)   = pure (ByteValConst x)
  traverse _ (ByteValInit x)    = pure (ByteValInit x)
  traverse f (ByteValCode ci x) = ByteValCode ci <$> f x
  traverse _ (ByteValLocal x)   = pure (ByteValLocal x)
