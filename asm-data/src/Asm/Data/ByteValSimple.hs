{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- can't move this to *cabal since it will break ghci when having this in the same package as the library
{-# LANGUAGE ImplicitPrelude #-}

module Asm.Data.ByteValSimple
  ( ByteValSimple(..)
  , byteValSimpleWord8
  , byteValSimpleAny
  , combine
  , final
  , final0
  , final255
  , byteValSimpleMaskedWord8
  , byteValSimpleToMaskedWord8
  , byteValSimpleLE
  , byteValSimpleBE
  ) where

import           Control.Monad
import           Data.Bits
import qualified Data.ByteString.Builder as BB
import           Data.Data               (Data)
import           Data.List
import           Data.Typeable           (Typeable)
import           Data.Word
import           Foreign.Storable

import           Asm.Data.BitList
import           Asm.Data.Word256

newtype ByteValSimple = ByteValSimple Word256 -- invariant: value can't be zero
  deriving (Data, Eq, Ord, Storable, Typeable)

instance Show ByteValSimple where
  show (ByteValSimple w) =
    let
      lz = countLeadingZeros w
      tz = countTrailingZeros w
    in
      if (255 - lz) == tz
        then show tz
        else
          if (maxBound `shiftR` (lz + tz)) `shiftL` tz == w
            then show tz ++ ".." ++ show (255 - lz)
            else case foldl' checkMask (Just (0, 0, map fromIntegral $ Asm.Data.BitList.toList w)) [0..7] of
              Just (v, m, _) -> show v ++ "*&" ++ show (complement m)
              Nothing        -> show $ Asm.Data.BitList.toList w
    where
      checkMask :: Maybe (Word8, Word8, [Word8]) -> Int -> Maybe (Word8, Word8, [Word8])
      checkMask Nothing _ = Nothing
      checkMask (Just (v, m, l)) b =
        case partition (`testBit` b) l of
          ([], _:_) -> Just (v, m, l)
          (_:_, []) -> Just (v .|. bit b, m, l)
          (l', r') ->
            if map (`clearBit` b) l' == r'
              then Just (v, m .|. bit b, r')
              else Nothing

byteValSimpleWord8 :: Word8 -> ByteValSimple
{-# INLINE byteValSimpleWord8 #-}
byteValSimpleWord8 b = ByteValSimple $ bit (fromIntegral b)

byteValSimpleAny :: ByteValSimple
{-# INLINE byteValSimpleAny #-}
byteValSimpleAny = ByteValSimple maxBound

combine :: ByteValSimple -> ByteValSimple -> Maybe ByteValSimple
combine (ByteValSimple a) (ByteValSimple b) =
  let
    ab = a .&. b
  in
    if ab == zeroBits
      then Nothing
      else Just $ ByteValSimple ab

final :: Word8 -> ByteValSimple -> Word8
final 0 (ByteValSimple w) = fromIntegral (countTrailingZeros w)
final 255 (ByteValSimple w) = complement $ fromIntegral (countLeadingZeros w)
final def (ByteValSimple w) =
  case checkPopulation w of
    PrNone       -> error "Asm.Data.ByteValSimple.final invariant: no bits set"
    (PrSingle x) -> fromIntegral x
    PrMany       -> minimumBy order $ map fromIntegral $ toList w
    PrAll        -> def
  where
    order :: Word8 -> Word8 -> Ordering
    order a b =
      mconcat
        [ popCount da `compare` popCount db
        , countLeadingZeros db `compare` countLeadingZeros da
        , if def < 128
            then a `compare` b
            else b `compare` a
        ]
      where
        da = def `xor` a
        db = def `xor` b

final0 :: ByteValSimple -> Word8
{-# INLINE final0 #-}
final0 (ByteValSimple w) = fromIntegral (countTrailingZeros w)

final255 :: ByteValSimple -> Word8
{-# INLINE final255 #-}
final255 (ByteValSimple w) = complement $ fromIntegral (countLeadingZeros w)

byteValSimpleLE :: ByteValSimple -> BB.Builder
{-# INLINE byteValSimpleLE #-}
byteValSimpleLE (ByteValSimple a) = word256LE a

byteValSimpleBE :: ByteValSimple -> BB.Builder
{-# INLINE byteValSimpleBE #-}
byteValSimpleBE (ByteValSimple a) = word256BE a

byteValSimpleMaskedWord8 :: Word8 -> Word8 -> ByteValSimple
byteValSimpleMaskedWord8 _ 0 = byteValSimpleAny
byteValSimpleMaskedWord8 v 255 = byteValSimpleWord8 v
byteValSimpleMaskedWord8 v m =
  ByteValSimple $ foldl' setBit zeroBits $ foldl' addUnknown [fromIntegral $ v .&. m] [0..7]
  where
    addUnknown vs b
      | m `testBit` b = vs
      | otherwise = vs ++ map (.|. bit b) vs

byteValSimpleToMaskedWord8 :: Word8 -> ByteValSimple -> (Word8, Word8)
byteValSimpleToMaskedWord8 def (ByteValSimple a)
  | a == maxBound = (0, 0)
  | otherwise =
    let
      vs = do
        v <- [0..255]
        m <- [0..255]
        guard (v .&. complement m == 0)
        let
          (ByteValSimple bvs) = byteValSimpleMaskedWord8 v m
        guard (complement a .&. bvs == zeroBits)
        return ((v, m), popCount m)
    in
      fst (minimumBy order vs)
  where
    order :: ((Word8, Word8), Int) -> ((Word8, Word8), Int) -> Ordering
    order ((va, _), pa) ((vb, _), pb) =
      mconcat
        [ pa `compare` pb
        , popCount da `compare` popCount db
        , countLeadingZeros db `compare` countLeadingZeros da
        , if def < 128
            then va `compare` vb
            else vb `compare` va
        ]
      where
        da = def `xor` va
        db = def `xor` vb
