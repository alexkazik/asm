{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Demo.C64.Image
  ( moduleC64Image
  ) where

import           Data.ByteString.Builder (word16LE)
import           Data.Vector             (Vector)
import qualified Data.Vector.Storable    as SV

import           Asm.C64

import           Demo.Multi.UnRLE
import           Demo.Output


imageRle :: Vector ByteValSimple
imageRle = SV.convert $ compressRLE $ createScreenAndBitmap $ withSlicesOf 8 8 (optimiseHiresBitmap . render1BitWidth [(lightGrey8, 0)] 1) $(imageWithUpdate "./image.png")
  where
    createScreenAndBitmap (a, b) = map byteValSimpleWord8 a ++ replicate 24 byteValSimpleAny ++ b

-- image is from http://csdb.dk/release/?id=58626

source :: Asm
source = $(asmFile "./demo.asm")

moduleC64Image :: [ModuleOutput]
moduleC64Image =
  let
    cr = compile source
    Just (poolStart, poolData) = lookup "out" (crPoolsWithData cr)
  in
    [ moduleOutput
        "c64-image"
        cr
        [ ("c64-image.prg", word16LE (fromIntegral poolStart) <> poolDataToByteStringBuilder 0 poolData)
        ]
    ]
