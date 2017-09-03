module Asm.Tools.Image.Internal
  ( Image(..)
  , createImage
  , toByteString
  ) where

import           Asm.Core.Prelude
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL

data Image =
  Image
    { imageWidth      :: !Int
    , imageHeight     :: !Int
    , imageOffsetX    :: !Int
    , imageOffsetY    :: !Int
    , imageFullWidth  :: !Int
    , imageFullHeight :: !Int
    , imageData       :: !ByteString
    }

createImage :: (Int, Int, ByteString) -> Image
{-# INLINE createImage #-}
createImage (w, h, img) =
  Image
    { imageWidth = w
    , imageHeight = h
    , imageOffsetX = 0
    , imageOffsetY = 0
    , imageFullWidth = w
    , imageFullHeight = h
    , imageData = img
    }

toByteString :: Image -> LByteString
{-# INLINEABLE toByteString #-}
toByteString Image{..} = BSL.fromChunks
  [
    slice ((y + imageOffsetY) * imageFullWidth + imageOffsetX) imageWidth imageData
  | y <- [0 .. imageHeight - 1]
  ]
  where
    slice :: Int -> Int -> ByteString -> ByteString
    slice o n s = BS.take n $ BS.drop o s
