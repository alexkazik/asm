module Asm.Core.Compiler
  ( compileGeneric
  , CompilerResult
  , crPoolsWithData
  , crPoolsStats
  , crDumpState
  , poolDataToByteString
  , poolDataToLazyByteString
  , poolDataToByteStringBuilder
  ) where

import           Asm.Core.Prelude
import qualified Data.ByteString                     as BS
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy                as BL
import qualified Data.Vector                         as V

import           Asm.Core.Data.Cpu
import           Asm.Core.Phase1.Compiler
import           Asm.Core.Phase2.Compiler
import           Asm.Core.Phase3.Compiler
import           Asm.Core.Phase4.Compiler
import           Asm.Core.Phase4.Data.CompilerResult
import           Asm.Data.ByteValSimple

compileGeneric :: (CpuSource c) => Source c -> CompilerResult c
compileGeneric blk = compile4 $ compile3 $ compile2 $ compile1 (fromSource blk)

poolDataToByteString :: Word8 -> Vector ByteValSimple -> ByteString
poolDataToByteString def = BS.pack . map (final def) . V.toList

poolDataToLazyByteString :: Word8 -> Vector ByteValSimple -> LByteString
poolDataToLazyByteString def = BL.pack . map (final def) . V.toList

poolDataToByteStringBuilder :: Word8 -> Vector ByteValSimple -> Builder
poolDataToByteStringBuilder def = lazyByteString . BL.pack . map (final def) . V.toList
