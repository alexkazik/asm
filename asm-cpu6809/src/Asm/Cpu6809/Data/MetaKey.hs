{-# LANGUAGE TemplateHaskell #-}

module Asm.Cpu6809.Data.MetaKey
  ( module Asm.Core.Data.MetaKey
  , metaCpu
  , metaCheckImm
  , metaCheckImm8
  , metaCheckImm16
  , metaCheckOfs
  , metaCheckOfs16
  , metaDP
  , metaX
  , metaY
  , metaU
  , metaS
  , metaOptimise
  ) where

import           Asm.Core.Data.MetaKey

mkCpuMetaKeys
  [ "metaCpu"
  , "metaCheckImm"
  , "metaCheckImm8"
  , "metaCheckImm16"
  , "metaCheckOfs"
  , "metaCheckOfs16"
  , "metaDP"
  , "metaX"
  , "metaY"
  , "metaU"
  , "metaS"
  , "metaOptimise"
  ]
