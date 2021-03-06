{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Asm.Cpu6502.Data.FunctionKey
  ( module Asm.Core.Data.FunctionKey
  , cpu6502FunctionKeys
  , fnCheckAddr8
  , fnCheckAddr16
  , fnConvertRel8
  , fnCheckJmpInd
  , fnCheckLaxImm
  ) where

import           Asm.Core.Data.FunctionKey

mkCpuFunctionKeys "cpu6502FunctionKeys"
  [ ("fnCheckAddr8", ":checkAddr8")
  , ("fnCheckAddr16", ":checkAddr16")
  , ("fnConvertRel8", ":convertRel8")
  , ("fnCheckJmpInd", ":checkJmpInd")
  , ("fnCheckLaxImm", ":checkLaxImm")
  ]
