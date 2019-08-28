{-# LANGUAGE NoImplicitPrelude #-}

module Asm.C64.Image.Embed
  ( imageWithUpdate
  , image
  ) where

import           Asm.Core.Prelude
import           Control.Monad.ST
import qualified Data.Vector.Storable         as SV
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed          as UV
import qualified Data.Vector.Unboxed.Mutable  as MUV
import qualified Language.Haskell.TH          as TH

import           Asm.Tools.Image.Embed

import           Asm.C64.Colors

imageWithUpdate :: FilePath -> TH.ExpQ
imageWithUpdate = embedImage convertImage True

image :: FilePath -> TH.ExpQ
image = embedImage convertImage False

convertImage :: Int -> SVector Word8 -> (UVector Word8, SVector Word8)
convertImage len oimg = runST $ do
  cmg <- MUV.new len
  img <- MSV.new (len*4)
  forM_ [0..len-1] $ \i -> do
    let
      i4 = i * 4
      (cc, r', g', b', a') =
        if (oimg SV.! (i4+3)) < 128
          then
            (transparent8, 0, 0, 0, 0)
          else
            let
              r = oimg SV.! (i4+0)
              g = oimg SV.! (i4+1)
              b = oimg SV.! (i4+2)
            in
              snd $
                minimumByEx (\x y -> fst x `compare` fst y) $
                map (diff r g b) c64colors
    MUV.write cmg i cc
    MSV.write img (i4+0) r'
    MSV.write img (i4+1) g'
    MSV.write img (i4+2) b'
    MSV.write img (i4+3) a'
  cmg' <- UV.unsafeFreeze cmg
  img' <- SV.unsafeFreeze img
  return (cmg', img')

diff :: Word8 -> Word8 -> Word8 -> (Word8, Word8, Word8, Word8, Word8) -> (Int, (Word8, Word8, Word8, Word8, Word8))
{-# INLINE diff #-}
diff r1 g1 b1 x@(_, r2, g2, b2, _) =(  abs (fromIntegral r1 - fromIntegral r2)
                                     + abs (fromIntegral g1 - fromIntegral g2)
                                     + abs (fromIntegral b1 - fromIntegral b2)
                                    , x)

c64colors :: [(Word8, Word8, Word8, Word8, Word8)]
{-# INLINE c64colors #-}
c64colors =
  [ ( 0, 0x00,0x00,0x00,0xff)
  , ( 1, 0xff,0xff,0xff,0xff)
  , ( 2, 0xae,0x34,0x26,0xff)
  , ( 3, 0x65,0xd6,0xd1,0xff)
  , ( 4, 0xb1,0x38,0xb4,0xff)
  , ( 5, 0x4a,0xc1,0x49,0xff)
  , ( 6, 0x3a,0x35,0xbd,0xff)
  , ( 7, 0xdc,0xeb,0x4a,0xff)
  , ( 8, 0xb2,0x52,0x15,0xff)
  , ( 9, 0x66,0x36,0x0a,0xff)
  , (10, 0xc3,0x73,0x67,0xff)
  , (11, 0x50,0x50,0x50,0xff)
  , (12, 0x83,0x83,0x83,0xff)
  , (13, 0x9c,0xf9,0x9d,0xff)
  , (14, 0x75,0x7a,0xe8,0xff)
  , (15, 0xb2,0xb2,0xb2,0xff)
  ]
