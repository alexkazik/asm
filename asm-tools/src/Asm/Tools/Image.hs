module Asm.Tools.Image
 ( -- * Asm.Tools.Image.Internal
   Image
 , imageWidth
 , imageHeight
 , toByteString
 , histogram
   -- * Asm.Tools.Image.RenderWide
 , render1BitWidth
 , render2BitWidth
   -- * Asm.Tools.Image
 , slicesOf
 , withSlicesOf
 , crop
 ) where

import           Asm.Core.Prelude

import           Asm.Tools.Image.Internal
import           Asm.Tools.Image.RenderWide

withSlicesOf :: Monoid m => Int -> Int -> (Image -> m) -> Image -> m
{-# INLINE withSlicesOf #-}
withSlicesOf w h fn img = mconcat $ map fn (slicesOf w h img)

slicesOf :: Int -> Int -> Image -> [Image]
{-# INLINEABLE slicesOf #-}
slicesOf w h img =
  [
    img
    { imageWidth = w
    , imageHeight = h
    , imageOffsetX = imageOffsetX img + x * w
    , imageOffsetY = imageOffsetY img + y * h
    }
  | y <- [0 .. (imageHeight img `div` h)-1]
  , x <- [0 .. (imageWidth img `div` w)-1]
  ]

crop :: Int -> Int -> Int -> Int -> Image -> Image
crop x y w h Image{..}
  | x >= 0 && y >= 0 && w > 0 && h > 0 && x + w <= imageWidth && y + h <= imageHeight =
      Image
        { imageWidth = w
        , imageHeight = h
        , imageOffsetX = imageOffsetX + x
        , imageOffsetY = imageOffsetY + y
        , ..
        }
  | otherwise =
      Image
        { imageWidth      = 0
        , imageHeight     = 0
        , imageOffsetX    = 0
        , imageOffsetY    = 0
        , imageFullWidth  = 0
        , imageFullHeight = 0
        , imageData       = mempty
        }
