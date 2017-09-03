module Asm.Tools.Image
 ( -- * Asm.Tools.Image.Internal
   Image
 , imageWidth
 , imageHeight
 , toByteString
   -- * Asm.Tools.Image
 , slicesOf
 , withSlicesOf
 , histogram
 ) where

import           Asm.Core.Prelude
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.IntMap.Strict       as IM

import           Asm.Tools.Image.Internal

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

histogram :: Image -> [Word8]
histogram img =
  map (fromIntegral . fst) $
  sortBy (comparing (Down . snd)) $
  IM.toList $
  BSL.foldr (\i -> IM.insertWith (+) (fromIntegral i) (1 :: Int)) IM.empty $
  toByteString img
