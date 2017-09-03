{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- can't move this to *cabal since it will break ghci when having this in the same package as the library
{-# LANGUAGE ImplicitPrelude #-}

module Asm.Data.ByteValSimple
  ( ByteValSimple(..)
  , byteValSimpleWord8
  , byteValSimpleAny
  , combine
  , final
  ) where

import           Data.Bits
import           Data.Data        (Data)
import           Data.List
import           Data.Typeable    (Typeable)
import           Data.Word
import           Foreign.Storable

import           Asm.Data.BitList
import           Asm.Data.Word256

newtype ByteValSimple = ByteValSimple Word256 -- invariant: value can't be zero
  deriving (Data, Eq, Storable, Typeable)

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
byteValSimpleWord8 b = ByteValSimple $ bit (fromIntegral b)

byteValSimpleAny :: ByteValSimple
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
