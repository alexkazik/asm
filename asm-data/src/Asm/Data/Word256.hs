{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE CPP #-}
-- can't move this to *cabal since it will break ghci when having this in the same package as the library
{-# LANGUAGE ImplicitPrelude #-}

module Asm.Data.Word256
  ( Word256(..)
  ) where

import           Data.Bits
import           Data.Data        (Data)
import           Data.Typeable    (Typeable)
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           Text.Printf

data Word256
  = Word256
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
  deriving (Data, Eq, Typeable)

instance Bounded Word256 where
  minBound = Word256 0 0 0 0
  maxBound = Word256 maxBound maxBound maxBound maxBound

instance Show Word256 where
  show (Word256 0 0 0 d) = printf "0x%016x" d
  show (Word256 0 0 c d) = printf "0x%016x%016x" c d
  show (Word256 0 b c d) = printf "0x%016x%016x%016x" b c d
  show (Word256 a b c d) = printf "0x%016x%016x%016x%016x" a b c d

instance Ord Word256 where
  (Word256 a b c d) `compare` (Word256 g h i j) =
    (a `compare` g) `mappend`
    (b `compare` h) `mappend`
    (c `compare` i) `mappend`
    (d `compare` j)

instance Bits Word256 where
  (Word256 a b c d) .&. (Word256 g h i j) =
    Word256 (a .&. g) (b .&. h) (c .&. i) (d .&. j)
  (Word256 a b c d) .|. (Word256 g h i j) =
    Word256 (a .|. g) (b .|. h) (c .|. i) (d .|. j)
  (Word256 a b c d) `xor` (Word256 g h i j) =
    Word256 (a `xor` g) (b `xor` h) (c `xor` i) (d `xor` j)
  complement (Word256 a b c d) = Word256 (complement a) (complement b) (complement c) (complement d)
  w `shift` n
    | n >= 256 || n <= -256 = zeroBits
    | otherwise = fromInteger' (toInteger' w `shift` n)
    where
    fromInteger' :: Integer -> Word256
    fromInteger' i =
      Word256
        (fromIntegral $ i `shiftR` 192)
        (fromIntegral $ i `shiftR` 128)
        (fromIntegral $ i `shiftR` 64)
        (fromIntegral $ i `shiftR` 0)
    toInteger' :: Word256 -> Integer
    toInteger' (Word256 a b c d) =
      (fromIntegral a `shiftL` 192) .|.
      (fromIntegral b `shiftL` 128) .|.
      (fromIntegral c `shiftL` 64) .|.
      (fromIntegral d `shiftL` 0)
  w `rotate` nn = (w `shift` n) .|. (w `shift` (n - 256))
    where
    n = nn .&. 255
  zeroBits = Word256 0 0 0 0
  bit n
    | n < 0 = Word256 0 0 0 0
    | n < 64 = Word256 0 0 0 (bit n)
    | n < 128 = Word256 0 0 (bit (n - 64)) 0
    | n < 192 = Word256 0 (bit (n - 128)) 0 0
    | n < 256 = Word256 (bit (n - 192)) 0 0 0
    | otherwise = Word256 0 0 0 0
  setBit (Word256 a b c d) n
    | n < 0 = Word256 a b c d
    | n < 64 = Word256 a b c (d `setBit` n)
    | n < 128 = Word256 a b (c `setBit` (n - 64)) d
    | n < 192 = Word256 a (b `setBit` (n - 128)) c d
    | n < 256 = Word256 (a `setBit` (n - 192)) b c d
    | otherwise = Word256 a b c d
  clearBit (Word256 a b c d) n
    | n < 0 = Word256 a b c d
    | n < 64 = Word256 a b c (d `clearBit` n)
    | n < 128 = Word256 a b (c `clearBit` (n - 64)) d
    | n < 192 = Word256 a (b `clearBit` (n - 128)) c d
    | n < 256 = Word256 (a `clearBit` (n - 192)) b c d
    | otherwise = Word256 a b c d
  testBit (Word256 a b c d) n
    | n < 0 = False
    | n < 64 = d `testBit` n
    | n < 128 = c `testBit` (n - 64)
    | n < 192 = b `testBit` (n - 128)
    | n < 256 = a `testBit` (n - 192)
    | otherwise = False
  bitSizeMaybe _ = Just 256
  isSigned _ = False
  popCount (Word256 a b c d) = popCount a + popCount b + popCount c + popCount d

instance FiniteBits Word256 where
  finiteBitSize _ = 256

instance Storable Word256 where
  sizeOf _ = 4 * sizeOf (0 :: Word64)
  alignment _ = alignment (0 :: Word64)
  peekByteOff ptr off =
    Word256
#ifdef WORDS_BIGENDIAN
      <$> peekByteOff ptr64 (off + 0 * size64)
      <*> peekByteOff ptr64 (off + 1 * size64)
      <*> peekByteOff ptr64 (off + 2 * size64)
      <*> peekByteOff ptr64 (off + 3 * size64)
#else
      <$> peekByteOff ptr64 (off + 3 * size64)
      <*> peekByteOff ptr64 (off + 2 * size64)
      <*> peekByteOff ptr64 (off + 1 * size64)
      <*> peekByteOff ptr64 (off + 0 * size64)
#endif
    where
    ptr64 :: Ptr Word64
    ptr64 = castPtr ptr
    size64 = sizeOf (0 :: Word64)
  pokeByteOff ptr off (Word256 a b c d) = do
#ifdef WORDS_BIGENDIAN
    pokeByteOff ptr64 (off + 0 * size64) a
    pokeByteOff ptr64 (off + 1 * size64) b
    pokeByteOff ptr64 (off + 2 * size64) c
    pokeByteOff ptr64 (off + 3 * size64) d
#else
    pokeByteOff ptr64 (off + 0 * size64) d
    pokeByteOff ptr64 (off + 1 * size64) c
    pokeByteOff ptr64 (off + 2 * size64) b
    pokeByteOff ptr64 (off + 3 * size64) a
#endif
    where
    ptr64 :: Ptr Word64
    ptr64 = castPtr ptr
    size64 = sizeOf (0 :: Word64)
