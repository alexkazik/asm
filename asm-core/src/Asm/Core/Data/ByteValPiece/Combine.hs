module Asm.Core.Data.ByteValPiece.Combine
 ( combine
 ) where

import           Asm.Core.Prelude
import qualified Data.Map.Strict            as M
import qualified Data.Vector                as V

import           Asm.Core.Data.ByteVal
import           Asm.Core.Data.ByteValPiece
import           Asm.Core.Data.Reference

data BVState c
  = BVState
    { bvsData  :: !(Vector (ByteVal c))
    , bvsNames :: !(Map Reference Int64)
    }

combine :: Int64 -> [ByteValPiece c] -> ByteValPiece c
combine start pieces = ByteValPiece (V.map removeLocal $ bvsData s) 1 Nothing (bvsNames s)
  where
    s = foldl' (applyState start) (BVState V.empty M.empty) (sortBy compareBVP pcs1 ++ sortBy compareBVP pcs2 ++ sortBy compareBVP pcs3)
    (pcs1, pcs2, pcs3) = foldl' partitionBVP ([], [], []) pieces
    removeLocal ByteValLocal{} = ByteValInit maxBound
    removeLocal x              = x

data BVPType
  = BVPRegular
  | BVPInitValue
  | BVPInitAll
  deriving (Eq, Ord)

partitionBVP :: ([ByteValPiece c], [ByteValPiece c], [ByteValPiece c]) -> ByteValPiece c -> ([ByteValPiece c], [ByteValPiece c], [ByteValPiece c])
partitionBVP (ia, ib, ic) bvpIn =
  case
    bool
      BVPRegular
      (foldr partitionBVP' BVPInitAll (bvpBytes bvp))
      (bvpAlign bvp == 1 && isNothing (bvpPage bvp))
  of
    BVPRegular   -> (bvp:ia, ib, ic)
    BVPInitValue -> (ia, bvp:ib, ic)
    BVPInitAll   -> (ia, ib, bvp:ic)
  where
    -- remove page restriction when size exceeds page
    bvp = case bvpPage bvpIn of
      Nothing -> bvpIn
      Just page ->
        if fromIntegral (V.length (bvpBytes bvpIn)) > page
          then bvpIn{bvpPage = Nothing}
          else bvpIn

partitionBVP' :: ByteVal c -> BVPType -> BVPType
partitionBVP' ByteValAny x = x
partitionBVP' (ByteValInit w) BVPInitAll
  | w == maxBound = BVPInitAll
  | otherwise = BVPInitValue
partitionBVP' ByteValInit{} x = x
partitionBVP' ByteValLocal{} x = x
partitionBVP' _ _ = BVPRegular


compareBVP :: ByteValPiece c -> ByteValPiece c -> Ordering
compareBVP
  ByteValPiece{bvpBytes = bytesA, bvpAlign = alignA, bvpPage = pageA, bvpNames = namesA}
  ByteValPiece{bvpBytes = bytesB, bvpAlign = alignB, bvpPage = pageB, bvpNames = namesB}
  | alignA /= alignB = alignB `compare` alignA
  | pageA /= pageB = pageB `compare` pageA
  | lenA /= lenB = lenB `compare` lenA
  | otherwise = namesB `compare` namesA
  where
    lenA = V.length bytesA
    lenB = V.length bytesB

applyState :: Int64 -> BVState c -> ByteValPiece c -> BVState c
applyState bvsStart BVState{..} ByteValPiece{..}
  = applyState' $ minimumByEx (comparing (\(_,x,_) -> x)) $ catMaybes
    [ combineVectorRight ofs (V.drop ofs bvsData) bvpBytes
      | ofs <- [0 .. length bvsData + fromIntegral (bvpAlign `max` fromMaybe 0 bvpPage) - 1]
      , (bvsStart + fromIntegral ofs) `mod` bvpAlign == 0
      , checkPage (fromIntegral ofs)
    ]
  where
    applyState' (ofs, _, ram) =
      BVState
        (V.concat [V.take ofs bvsData, V.replicate (ofs - length bvsData) ByteValAny, ram, V.drop (ofs + length ram) bvsData])
        (M.union (M.map (bvsStart + fromIntegral ofs + ) bvpNames) bvsNames)
    checkPage ofs = case bvpPage of
      Nothing   -> True
      Just page -> ((bvsStart + ofs) `div` page) == ((bvsStart + ofs + fromIntegral (length bvpBytes) - 1) `div` page)



combineVectorRight
  :: Int
  -> Vector (ByteVal c)
  -> Vector (ByteVal c)
  -> Maybe (Int, Int, Vector (ByteVal c))
combineVectorRight ofs = combineVectorRight' 0 V.empty
  where
    combineVectorRight' num done xs ys
      | V.null ys = Just (ofs, num, done)
      | V.null xs = Just (ofs, num + V.foldl' anyToX 0 ys, done V.++ ys)
      | otherwise =
          case combineL (V.head xs) (V.head ys) of
            Nothing     -> Nothing
            Just (z, n) -> combineVectorRight' (n+num) (done `V.snoc` z) (V.tail xs) (V.tail ys)
    anyToX num' ByteValAny       = num'
    anyToX num' (ByteValConst x) = num' + 320 - popCount x
    anyToX num' ByteValInit{}    = num' + 320 - 1
    anyToX num' ByteValCode{}    = num' + 320 - 1
    anyToX num' ByteValLocal{}   = num' + 320 - 256


combineL :: ByteVal c -> ByteVal c -> Maybe (ByteVal c, Int)
combineL x ByteValAny = Just (x, 0)
combineL ByteValAny x@(ByteValConst b) = Just (x, 320 - popCount b)
combineL ByteValAny x@ByteValInit{}    = Just (x, 320 - 1)
combineL ByteValAny x@ByteValCode{}    = Just (x, 320 - 1)
combineL ByteValAny x@ByteValLocal{}   = Just (x, 320 - 256)
combineL (ByteValConst a) (ByteValConst b) =
  if c == zeroBits
    then Nothing
    else Just (ByteValConst c, max 0 (popCount a - popCount b))
  where
    c = a .&. b
combineL (ByteValLocal a) (ByteValLocal b) =
  if not (match a b) && not (match b a)
    then Just (ByteValLocal (a <> b), 0)
    else Nothing
  where
    match :: Set [Text] -> Set [Text] -> Bool
    match c d = any (isPfx c) d
    isPfx :: Set [Text] -> [Text] -> Bool
    isPfx e f = any (isPrefixOf f) e
combineL _ _ = Nothing
