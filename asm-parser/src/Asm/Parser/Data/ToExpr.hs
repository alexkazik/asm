{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Asm.Parser.Data.ToExpr
  ( ToExpr (..)
  ) where

import           Asm.Core.Prelude
import           Data.List.Split

import           Asm.Core.Data.ByteVal
import           Asm.Core.Data.Ternary
import           Asm.Core.SourcePos
import           Asm.Data.ByteValSimple

import           Asm.Parser.Data.LabelName
import           Asm.Parser.Data.MagicValue
import           Asm.Parser.Data.PExpr
import           Asm.Parser.Data.ToByte
import           Asm.Parser.Parser.Tools


class ToExpr pe a where
  toExpr :: CpuParser c ps pe => SourcePos -> a -> PExpr pe

instance ToExpr pe (PExpr pe) where
  toExpr loc e = (loc, PETraceStep e)

instance ToExpr pe LabelName where
  toExpr loc lab =
    let
      (h:t) = splitOn "." (fromLabelName lab)
      labE = (loc, PELabelId (LabelIdValue h))
      deref e s = (loc, PEDerefStruct e (LabelIdValue s))
    in
      foldl' deref labE t

instance ToExpr pe MagicValue where
  toExpr loc v = (loc, PEMagicValue $ fromMagicValue v)

instance ToExpr pe Int64 where
  toExpr loc x = (loc, PEConstInt x)

instance ToExpr pe Int where
  toExpr loc x = (loc, PEConstInt $ fromIntegral x)

instance ToExpr pe Word64 where
  toExpr loc x = (loc, PEConstInt $ fromIntegral x)

instance ToExpr pe Word where
  toExpr loc x = (loc, PEConstInt $ fromIntegral x)

instance ToExpr pe TInt64 where
  toExpr loc x = (loc, PEConstMaskedInt x)

-- from ToByte

instance ToExpr pe Int8 where
  toExpr = toByte

instance ToExpr pe Word8 where
  toExpr = toByte

instance ToExpr pe Char where
  toExpr = toByte

instance ToExpr pe (ByteVal (PExpr pe)) where
  toExpr = toByte

instance ToExpr pe ByteValSimple where
  toExpr = toByte

instance ToExpr pe (Maybe ByteValSimple) where
  toExpr = toByte
