module Asm.Core.Data.ByteValPiece
 ( ByteValPiece(..)
 , dumpByteValPiece
 ) where

import           Asm.Core.Prelude
import qualified Data.Map.Strict         as M
import qualified Data.Vector             as V

import           Asm.Core.Data.ByteVal
import           Asm.Core.Data.Reference

data ByteValPiece c
  = ByteValPiece
    { bvpBytes :: !(Vector (ByteVal c))
    , bvpAlign :: !Int64
    , bvpPage  :: !(Maybe Int64)
    , bvpNames :: !(Map Reference Int64)
    }
  deriving (Eq, Show)

instance Eq c => Ord (ByteValPiece c) where
  a `compare` b =
    mconcat
      [ V.length (bvpBytes a) `compare` V.length (bvpBytes b)
      , bvpNames a `compare` bvpNames b
      ]

dumpByteValPiece :: Show c => ByteValPiece c -> String
dumpByteValPiece ByteValPiece{bvpBytes, bvpAlign, bvpPage, bvpNames}
  | not $ M.null $ M.filter (== 0) bvpNames =
      "        $BVP: Align " ++ show bvpAlign ++ ", " ++ "Page " ++ show bvpPage ++ "\n" ++
      V.ifoldr dumpByteValPiece' "" bvpBytes
  | otherwise = show bvpBytes ++ " ; " ++ show bvpNames ++ "\n"
      where
        dumpByteValPiece' ofs byte out =
          showNames (M.toList (M.filter (== fromIntegral ofs) bvpNames)) ++
          "            " ++ show byte ++ "\n" ++ out
        showNames []         = "" :: String
        showNames ((x,_):xs) = "          " ++ show x ++ "\n" ++ showNames xs
