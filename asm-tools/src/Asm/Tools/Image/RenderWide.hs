{-# LANGUAGE TupleSections #-}

module Asm.Tools.Image.RenderWide
 ( render1BitWidth
 , render2BitWidth
 ) where

import           Asm.Core.Prelude
import           Control.Monad.ST
import qualified Data.ByteString.Lazy           as BL
import qualified Data.ByteString.Unsafe         as BS
import qualified Data.Vector.Unboxed            as UV
import qualified Data.Vector.Unboxed.Mutable    as MUV

import           Asm.Core.Control.CompilerError
import           Asm.Tools.Image.Internal

render1BitWidth :: [(Word8, Int)] -> Int -> Image -> ((Maybe Word8, Maybe Word8), UVector Word8)
render1BitWidth prefColors def img =
  let
    ([c0, c1], res) = renderInternal 1 prefColors def img
  in
    ((c0, c1), res)

render2BitWidth :: [(Word8, Int)] -> Int -> Image -> ((Maybe Word8, Maybe Word8, Maybe Word8, Maybe Word8), UVector Word8)
render2BitWidth prefColors def img =
  let
    ([c0, c1, c2, c3], res) = renderInternal 2 prefColors def img
  in
    ((c0, c1, c2, c3), res)

renderInternal :: Int -> [(Word8, Int)] -> Int -> Image -> ([Maybe Word8], UVector Word8)
{-# INLINE renderInternal #-}
renderInternal pixelWidth prefColors def img = runST $ do
  let
    -- calculate num picked colors and (complement) mask
    pixelColorCount = bit pixelWidth
    pixelMask = pixelColorCount - 1
    -- convert image to LByteString
    imgBS = BL.toStrict $ toByteString img
    -- create histogram
    hist = histogram' imgBS
  -- create new vectors
  cmap <- MUV.new 256
  MUV.set cmap (-1) -- (remember for each color that no position is picked: -1, theoretical positions: 0..15)
  pcmap <- MUV.new pixelColorCount
  MUV.set pcmap (False, 0, False) -- (no color assigned yet)
  -- go over all preferred colors and assign them if used and the spot is still free (or forced)
  todo <- forM (prefColors ++ map (, def) hist) $ \(c, p') -> do
    let
      p = p' .&. pixelMask
      forced = (p' .&. 0xf0) /= 0
    cUsed <- MUV.read cmap (fromIntegral c)
    if (cUsed /= (-1) || c `notElem` hist) && not forced
      then return Nothing -- color is already used OR color is not in the image (and not forced)
      else do
        (pUsed, _, _) <- MUV.read pcmap p
        if | not pUsed -> do
              -- position is not yet used, regular assignment
              when (cUsed == (-1)) $
                MUV.write cmap (fromIntegral c) p
              MUV.write pcmap p (True, c, c `elem` hist)
              return Nothing
           | forced -> do
              -- is a forced assignment
              when (cUsed == (-1)) $
                MUV.write cmap (fromIntegral c) p
              return Nothing
           | otherwise ->
              -- space is taken, remember for free assignment
              return $ Just (c, p)
  -- go over all positions and fill up with not yet assigned colors
  pcmap' <- UV.freeze pcmap
  let
    fillTodo ((c, _):ts) (p, (False, _, _)) = do
      MUV.write cmap (fromIntegral c) p
      MUV.write pcmap p (True, c, True)
      return ts
    fillTodo ts _ = return ts
  todo' <- foldM fillTodo (catMaybes todo) (zip [0..] $ UV.toList pcmap')
  -- assign all sill not yet assigned colors as they wish
  -- this WILL collide and is only executed to produce valid output, even if the image will look weried
  forM_ todo' $ \(c, p) -> do
    MUV.write cmap (fromIntegral c) p
  -- read color mapping and (primary color)
  cmap' <- UV.unsafeFreeze cmap
  pcmap'' <- UV.unsafeFreeze pcmap
  return
    ( map (\(_, c, out) -> bool Nothing (Just c) out) $ UV.toList pcmap''
    , case pixelWidth of
        1 -> render1 imgBS cmap'
        2 -> render2 imgBS cmap'
        4 -> render4 imgBS cmap'
        _ -> [printInternalError|render: pixel width $pixelWidth is not supported|]
    )

render1 :: ByteString -> UVector Int -> UVector Word8
{-# INLINE render1 #-}
render1 imageBS cmap = runST $ do
  let
    bytes = length imageBS `div` 8
  out <- MUV.new bytes
  forM_ [0 .. bytes-1] $ \ofs -> do
    MUV.write out ofs
      $ fromIntegral
      (   (cmap `UV.unsafeIndex` fromIntegral (imageBS `BS.unsafeIndex` (ofs*8 + 0))) `shiftL` 7
      .|. (cmap `UV.unsafeIndex` fromIntegral (imageBS `BS.unsafeIndex` (ofs*8 + 1))) `shiftL` 6
      .|. (cmap `UV.unsafeIndex` fromIntegral (imageBS `BS.unsafeIndex` (ofs*8 + 2))) `shiftL` 5
      .|. (cmap `UV.unsafeIndex` fromIntegral (imageBS `BS.unsafeIndex` (ofs*8 + 3))) `shiftL` 4
      .|. (cmap `UV.unsafeIndex` fromIntegral (imageBS `BS.unsafeIndex` (ofs*8 + 4))) `shiftL` 3
      .|. (cmap `UV.unsafeIndex` fromIntegral (imageBS `BS.unsafeIndex` (ofs*8 + 5))) `shiftL` 2
      .|. (cmap `UV.unsafeIndex` fromIntegral (imageBS `BS.unsafeIndex` (ofs*8 + 6))) `shiftL` 1
      .|. (cmap `UV.unsafeIndex` fromIntegral (imageBS `BS.unsafeIndex` (ofs*8 + 7))) `shiftL` 0
      )
  UV.unsafeFreeze out

render2 :: ByteString -> UVector Int -> UVector Word8
{-# INLINE render2 #-}
render2 imageBS cmap = runST $ do
  let
    bytes = length imageBS `div` 8
  out <- MUV.new bytes
  forM_ [0 .. bytes-1] $ \ofs -> do
    MUV.write out ofs
      $ fromIntegral
      (   (cmap `UV.unsafeIndex` fromIntegral (imageBS `BS.unsafeIndex` (ofs*8 + 0))) `shiftL` 6
      .|. (cmap `UV.unsafeIndex` fromIntegral (imageBS `BS.unsafeIndex` (ofs*8 + 2))) `shiftL` 4
      .|. (cmap `UV.unsafeIndex` fromIntegral (imageBS `BS.unsafeIndex` (ofs*8 + 4))) `shiftL` 2
      .|. (cmap `UV.unsafeIndex` fromIntegral (imageBS `BS.unsafeIndex` (ofs*8 + 6))) `shiftL` 0
      )
  UV.unsafeFreeze out

render4 :: ByteString -> UVector Int -> UVector Word8
{-# INLINE render4 #-}
render4 imageBS cmap = runST $ do
  let
    bytes = length imageBS `div` 8
  out <- MUV.new bytes
  forM_ [0 .. bytes-1] $ \ofs -> do
    MUV.write out ofs
      $ fromIntegral
      (   (cmap `UV.unsafeIndex` fromIntegral (imageBS `BS.unsafeIndex` (ofs*8 + 0))) `shiftL` 4
      .|. (cmap `UV.unsafeIndex` fromIntegral (imageBS `BS.unsafeIndex` (ofs*8 + 4))) `shiftL` 0
      )
  UV.unsafeFreeze out
