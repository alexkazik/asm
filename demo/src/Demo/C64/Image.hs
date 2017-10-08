-- can't move this to *cabal since it will break ghci when having this in the same package as the library
{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Demo.C64.Image
  ( moduleC64Image
  ) where

import           Data.ByteString.Builder
import           Data.Semigroup
import           Data.Vector             (Vector)
import qualified Data.Vector.Storable    as SV

import           Asm.C64

import           Demo.Output


imageRle :: Vector ByteValSimple
imageRle = SV.convert $ compressRLE $ createScreenAndBitmap $ withSlicesOf 8 8 (renderHiresL 15) $(imageWithUpdate "./image.png")
  where
    createScreenAndBitmap (a, b) = map byteValSimpleWord8 a ++ replicate 24 byteValSimpleAny ++ b

-- image is from http://csdb.dk/release/?id=58626

source :: Asm
source = $(asmFile "./demo.asm")

moduleC64Image :: (String, [(FilePath, Builder)])
moduleC64Image =
  let
    cr = compile source
    Just (poolStart, poolData) = lookup "out" (crPoolsWithData cr)
  in
    (
      moduleOutput "c64-image" cr
    , [ ("c64-image.prg", word16LE (fromIntegral poolStart) <> poolDataToByteStringBuilder 0 poolData)
      ]
    )
