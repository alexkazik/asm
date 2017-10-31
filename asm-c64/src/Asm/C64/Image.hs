module Asm.C64.Image
  ( module Asm.Tools.Image
  , module Asm.C64.Image.Embed
  , renderHiresBg
  , renderHiresFg
  , renderHires
  , renderHiresL
  ) where

import           Asm.Core.Prelude
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.Unsafe as BS

import           Asm.Data.ByteValSimple
import           Asm.Tools.Image

import           Asm.C64.Image.Embed

renderHiresBg :: Word8 -> Image -> [Word8]
renderHiresBg bg img = go (toByteString img)
  where
    go si =
      let
        (by', re) = BL.splitAt 8 si
        by = BL.toStrict by'
      in
        if BS.length by /= 8
          then []
          else
            (
              (if (by `BS.unsafeIndex` 0) == bg then 0 else 0x80) .|.
              (if (by `BS.unsafeIndex` 1) == bg then 0 else 0x40) .|.
              (if (by `BS.unsafeIndex` 2) == bg then 0 else 0x20) .|.
              (if (by `BS.unsafeIndex` 3) == bg then 0 else 0x10) .|.
              (if (by `BS.unsafeIndex` 4) == bg then 0 else 0x08) .|.
              (if (by `BS.unsafeIndex` 5) == bg then 0 else 0x04) .|.
              (if (by `BS.unsafeIndex` 6) == bg then 0 else 0x02) .|.
              (if (by `BS.unsafeIndex` 7) == bg then 0 else 0x01)
            ) : go re

renderHiresFg :: Word8 -> Image -> [Word8]
renderHiresFg fg img = go (toByteString img)
  where
    go si =
      let
        (by', re) = BL.splitAt 8 si
        by = BL.toStrict by'
      in
        if BS.length by /= 8
          then []
          else
            (
              (if (by `BS.unsafeIndex` 0) == fg then 0x80 else 0) .|.
              (if (by `BS.unsafeIndex` 1) == fg then 0x40 else 0) .|.
              (if (by `BS.unsafeIndex` 2) == fg then 0x20 else 0) .|.
              (if (by `BS.unsafeIndex` 3) == fg then 0x10 else 0) .|.
              (if (by `BS.unsafeIndex` 4) == fg then 0x08 else 0) .|.
              (if (by `BS.unsafeIndex` 5) == fg then 0x04 else 0) .|.
              (if (by `BS.unsafeIndex` 6) == fg then 0x02 else 0) .|.
              (if (by `BS.unsafeIndex` 7) == fg then 0x01 else 0)
            ) : go re

renderHires :: Word8 -> Image -> (Word8, [ByteValSimple])
renderHires prefBg img =
  case histogram img of
    [col] -> (col .|. (col `shiftL` 4), replicate 8 byteValSimpleAny)
    (c0:c1:_) ->
          if c1 == prefBg
            then (c1 .|. (c0 `shiftL` 4), map byteValSimpleWord8 $ renderHiresBg c1 img)
            else (c0 .|. (c1 `shiftL` 4), map byteValSimpleWord8 $ renderHiresBg c0 img)
    [] -> (prefBg .|. (prefBg `shiftL` 4), replicate 8 byteValSimpleAny)

renderHiresL :: Word8 -> Image -> ([Word8], [ByteValSimple])
renderHiresL prefBg img =
  case histogram img of
    [col] -> ([col .|. (col `shiftL` 4)], replicate 8 byteValSimpleAny)
    (c0:c1:_) ->
          if c1 == prefBg
            then ([c1 .|. (c0 `shiftL` 4)], map byteValSimpleWord8 $ renderHiresBg c1 img)
            else ([c0 .|. (c1 `shiftL` 4)], map byteValSimpleWord8 $ renderHiresBg c0 img)
    [] -> ([prefBg .|. (prefBg `shiftL` 4)], replicate 8 byteValSimpleAny)
