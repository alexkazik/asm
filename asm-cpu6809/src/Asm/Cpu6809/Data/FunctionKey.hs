{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Asm.Cpu6809.Data.FunctionKey
  ( module Asm.Core.Data.FunctionKey
  , cpu6809FunctionKeys
  , fnCheckRelIs8
  , fnConvertRel8
  , fnConvertRel16
  , fnCheckAddr8
  , fnCheckAddr16
  , fnCheckOffsetZero
  ) where

import           Asm.Core.Data.FunctionKey

mkCpuFunctionKeys "cpu6809FunctionKeys"
  [ ("fnCheckRelIs8", ":checkRelIs8")
  , ("fnConvertRel8", ":convertRel8")
  , ("fnConvertRel16", ":convertRel16")
  , ("fnCheckAddr8", ":checkAddr8")
  , ("fnCheckAddr16", ":checkAddr16")
  , ("fnCheckOffsetZero", ":checkOffsetZero")
  ]
