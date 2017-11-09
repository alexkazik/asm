module Asm.C64.Image
  ( module Asm.Tools.Image
  , module Asm.C64.Image.Embed
  , optimiseHiresBitmap
  ) where

import           Asm.Core.Prelude
import qualified Data.Vector.Unboxed    as UV

import           Asm.Data.ByteValSimple
import           Asm.Tools.Image

import           Asm.C64.Image.Embed

optimiseHiresBitmap :: ((Maybe Word8, Maybe Word8), UVector Word8) -> ([Word8], [ByteValSimple])
optimiseHiresBitmap (cols, res) =
  case cols of
    (Just c0, Just c1)
      | c0 == c1       -> ([c0 .|. (c1 `shiftL` 4)], replicate (UV.length res) byteValSimpleAny)
      | otherwise      -> ([c0 .|. (c1 `shiftL` 4)], map byteValSimpleWord8 $ UV.toList res)
    (Just c0, Nothing) -> ([c0 .|. (c0 `shiftL` 4)], replicate (UV.length res) byteValSimpleAny)
    (Nothing, Just c1) -> ([c1 .|. (c1 `shiftL` 4)], replicate (UV.length res) byteValSimpleAny)
    (Nothing, Nothing) -> ([0], []) -- can only happen with an empty image, color choice is random
