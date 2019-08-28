{-# LANGUAGE NoImplicitPrelude #-}

module Asm.Core.Compiler
  ( compileGeneric
  , compileGeneric'
  , CompilerResult
  , crPoolsWithData
  , crPoolsStats
  , crDumpState
  , poolDataToByteString
  , poolDataToLazyByteString
  , poolDataToByteStringBuilder
  ) where

import           Asm.Core.Prelude
import           Control.Arrow
import qualified Data.ByteString                     as BS
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy                as BL
import qualified Data.Vector                         as V

import           Asm.Core.Control.CompilerError
import           Asm.Core.Data.CompilerError
import           Asm.Core.Data.Cpu
import           Asm.Core.Phase1.Compiler
import           Asm.Core.Phase2.Compiler
import           Asm.Core.Phase3.Compiler
import           Asm.Core.Phase4.Compiler
import           Asm.Core.Phase4.Data.CompilerResult
import           Asm.Data.ByteValSimple

compileGeneric :: (CpuSource c) => Source c -> Either [(String, [String])] (CompilerResult c)
compileGeneric blk =
  left convertCompilerError $
    runError $
      compile1 (fromSource blk) >>= compile2 >>= compile3 >>= compile4

compileGeneric' :: (CpuSource c) => Source c -> CompilerResult c
compileGeneric' = either printCompilerError id . compileGeneric

poolDataToByteString :: Word8 -> Vector ByteValSimple -> ByteString
poolDataToByteString def = BS.pack . map (final def) . V.toList

poolDataToLazyByteString :: Word8 -> Vector ByteValSimple -> LByteString
poolDataToLazyByteString def = BL.pack . map (final def) . V.toList

poolDataToByteStringBuilder :: Word8 -> Vector ByteValSimple -> Builder
poolDataToByteStringBuilder def = lazyByteString . BL.pack . map (final def) . V.toList
