{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Asm.Parser.Data.ToByte
  ( ToByte (..)
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Control.CompilerError
import           Asm.Core.Data.ByteVal
import           Asm.Core.Data.Ternary
import           Asm.Core.SourcePos
import           Asm.Data.ByteValSimple

import           Asm.Parser.Data.PExpr
import           Asm.Parser.Parser.Tools


class ToByte pe a where
  toByte :: CpuParser c ps pe => SourcePos -> a -> PExpr pe

instance ToByte pe Int8 where
  toByte loc x = (loc, PEByteVal (ByteValConst $ bit $ fromIntegral (fromIntegral x :: Word8)))

instance ToByte pe Word8 where
  toByte loc x = (loc, PEByteVal (ByteValConst $ bit $ fromIntegral x))

instance ToByte pe Char where
  toByte loc x =
    if ox < 0 || ox > 0xff
      then $printError [([loc], "Char out of range")]
      else (loc, PEByteVal (ByteValConst $ bit ox))
    where
      ox = ord x

instance ToByte pe (ByteVal (PExpr pe)) where
  toByte loc v = (loc, PEByteVal v)

instance ToByte pe ByteValSimple where
  toByte loc (ByteValSimple v) = (loc, PEByteVal $ ByteValConst v)

instance ToByte pe (Maybe ByteValSimple) where
  toByte loc Nothing                  = (loc, PEByteVal ByteValAny)
  toByte loc (Just (ByteValSimple v)) = (loc, PEByteVal $ ByteValConst v)

instance ToByte pe TInt64 where
  toByte loc x =
    bool
      ($printError [([loc], "TInt64 out of range")])
      (loc, PEByteVal $ byteValMaskedWord8 ByteValIsConst (fromIntegral v) (fromIntegral m))
      (isTInt64inRange 0 255 0xff x)
    where
      (v, m) = tValueAndMask x

instance ToByte pe Int64 where
  toByte = toInt "Int64"

instance ToByte pe Int where
  toByte = toInt "Int"

instance ToByte pe Word64 where
  toByte = toInt "Word64"

instance ToByte pe Word where
  toByte = toInt "Word"

toInt :: (CpuParser c ps pe, Integral i) => String -> SourcePos -> i -> PExpr pe
toInt name loc i =
  if i < 0 || i > 0xff
    then $printError [([loc], name ++ " out of range")]
    else (loc, PEByteVal (ByteValConst $ bit $ fromIntegral i))
