{-# LANGUAGE TemplateHaskellQuotes #-}

module Asm.Tools.Image.Embed
  ( embedImage
  ) where

import           Asm.Core.Prelude
import           Codec.Picture            as Juicy
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import           Data.ByteString.Unsafe
import qualified Data.Vector.Storable     as SV
import qualified Data.Vector.Unboxed      as UV
import qualified Language.Haskell.TH      as TH
import           System.Directory
import           System.IO.Unsafe

import           Asm.Core.File
import           Asm.Core.SourcePos

import           Asm.Tools.Image.Internal (createImage)

embedImage :: (Int -> SV.Vector Word8 -> (UV.Vector Word8, SV.Vector Word8)) -> Bool -> FilePath -> TH.Q TH.Exp
{-# INLINE embedImage #-}
embedImage conv doUpdate fp = do
  fp' <- getRelativeFilePathQ True fp

  TH.runIO $ do
    origFile <- BS.readFile fp'

    let
      image = case decodePng origFile of
        Left err  -> printError [(spInternal, err)]
        Right img -> convertRGBA8 img

      (convertedData, imageData') = conv (imageWidth image * imageHeight image) (imageData image)

      newFile = LBS.toStrict $ encodePng (Juicy.Image (imageWidth image) (imageHeight image) imageData' :: Juicy.Image PixelRGBA8)

    when
      (doUpdate && origFile /= newFile) $ do
        removeFile fp'
        BS.writeFile fp' newFile

    return $
      TH.VarE 'createImage `TH.AppE`
        TH.TupE
          [ TH.LitE $ TH.IntegerL $ fromIntegral $ imageWidth image
          , TH.LitE $ TH.IntegerL $ fromIntegral $ imageHeight image
          , bsToExp $ UV.toList convertedData
          ]
  where
  bsToExp :: [Word8] -> TH.Exp
  bsToExp bs =
    TH.VarE 'unsafePerformIO
      `TH.AppE` (TH.VarE 'unsafePackAddressLen
        `TH.AppE` TH.LitE (TH.IntegerL $ fromIntegral $ length bs)
        `TH.AppE` TH.LitE (TH.StringPrimL bs))
