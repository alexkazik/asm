module Asm.Cpu6502.Data.MetaKey
  ( module Asm.Core.Data.MetaKey
  , metaCpu
  , metaCheckImm
  , metaCheckImm8
  , metaOptimise
  ) where

import           Asm.Core.Data.MetaKey

mkCpuMetaKeys
  [ "metaCpu"
  , "metaCheckImm"
  , "metaCheckImm8"
  , "metaOptimise"
  ]
