{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Asm.Tools.Image.Internal
  ( Image(..)
  , createImage
  , toByteString
  , histogram
  , histogram'
  ) where

import           Asm.Core.Prelude
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict   as IM

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
toByteString Image{..} = BL.fromChunks
  [
    slice ((y + imageOffsetY) * imageFullWidth + imageOffsetX) imageWidth imageData
  | y <- [0 .. imageHeight - 1]
  ]
  where
    slice :: Int -> Int -> ByteString -> ByteString
    slice o n s = BS.take n $ BS.drop o s

histogram :: Image -> [Word8]
histogram img =
  map (fromIntegral . fst) $
  sortBy (comparing (Down . snd)) $
  IM.toList $
  BL.foldr (\i -> IM.insertWith (+) (fromIntegral i) (1 :: Int)) IM.empty $
  toByteString img

-- only for internal use
histogram' :: ByteString -> [Word8]
{-# INLINE histogram' #-}
histogram' img =
  map (fromIntegral . fst) $
  sortBy (comparing (Down . snd)) $
  IM.toList $
  BS.foldr (\i -> IM.insertWith (+) (fromIntegral i) (1 :: Int)) IM.empty $
  img
