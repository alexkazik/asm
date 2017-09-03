module Asm.Tools.Data.RLE
  ( compressRLE
  ) where

import           Asm.Core.Prelude
import qualified Data.Vector            as V
import qualified Data.Vector.Storable   as SV

import           Asm.Data.ByteValSimple

compressRLE :: [ByteValSimple] -> SV.Vector ByteValSimple
compressRLE ivlist = addBuf (foldl' optimiseRle (SV.empty, SV.empty) combined) `SV.snoc` byteValSimpleWord8 0
  where
    combined = comb $ map ((,) 1) ivlist

type NumIVS = (Int, ByteValSimple)

comb :: [NumIVS] -> Vector NumIVS
comb (x@(xc,xv):y@(yc,yv):zs) =
  case combine xv yv of
    Nothing  -> x `V.cons` comb (y:zs)
    Just xyv -> comb ((xc+yc, xyv):zs)
comb [x] = V.singleton x
comb [] = V.empty

optimiseRle :: (SV.Vector ByteValSimple, SV.Vector ByteValSimple) -> NumIVS -> (SV.Vector ByteValSimple, SV.Vector ByteValSimple)
optimiseRle ob@(out, buf) (num, val)
  | num > 129 = optimiseRle (addBuf ob `SV.snoc` byteValSimpleWord8 0x7f `SV.snoc` val, SV.empty) (num - 129, val)
  | num >= 3 = (addBuf ob `SV.snoc` byteValSimpleWord8 (fromIntegral (num-2)) `SV.snoc` val, SV.empty)
  | otherwise = (out, buf ++ SV.replicate num val)

addBuf :: (SV.Vector ByteValSimple, SV.Vector ByteValSimple) -> SV.Vector ByteValSimple
addBuf (out, buf)
  | SV.null buf = out
  | otherwise =
      let
        (buf1, buf2) = SV.splitAt 128 buf
      in
        addBuf (out ++ (byteValSimpleWord8 (0x80 .|. fromIntegral (SV.length buf1 - 1)) `SV.cons` buf1), buf2)
