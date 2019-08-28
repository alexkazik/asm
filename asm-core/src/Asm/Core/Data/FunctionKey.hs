{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Asm.Core.Data.FunctionKey
  ( module Asm.Core.Data.FunctionKey.Internal
  , compilerFunctionKeys
  -- Address
  , fnAddr
  , fnAddrByte
  , fnAddrCode
  , fnBank
  , fnSize
  , fnByte
  , fnCode
  , fnAs
  -- Boolean
  , opLT
  , opLE
  , opGT
  , opGE
  , opEQ
  , opNE
  , opNot
  , opLAND
  , opLOR
  , opLXOR
  -- Check
  , fnCheckData8
  , fnCheckData8Unsigned
  , fnCheckData8Signed
  , fnCheckData8Both
  -- Data
  , opConcat
  , fnFill
  -- Integer
  , opPLUS
  , opMINUS
  , opMUL
  , opDIV
  , opMOD
  , opCOM
  , opAND
  , opOR
  , opXOR
  , opShiftL
  , opShiftR
  , fnRotate
  , opSetCare
  , opAddDoNotCare
  ) where

import           Asm.Core.Data.FunctionKey.Internal

mkCompilerFunctionKeys "compilerFunctionKeys"
  -- Address
  [ ("fnAddr", "Addr")
  , ("fnAddrByte", "Addr:Byte")
  , ("fnAddrCode", "Addr:Code")
  , ("fnBank", "Bank")
  , ("fnSize", "Size")
  , ("fnByte", "Byte")
  , ("fnCode", "Code")
  , ("fnAs", "As")
  -- Boolean
  , ("opLT", "<")
  , ("opLE", "<=")
  , ("opGT", ">")
  , ("opGE", ">=")
  , ("opEQ", "==")
  , ("opNE", "!=")
  , ("opNot", "!")
  , ("opLAND", "&&")
  , ("opLOR", "||")
  , ("opLXOR", "^^")
  -- Check
  , ("fnCheckData8", ":CheckData8")
  , ("fnCheckData8Unsigned", ":CheckData8Unsigned")
  , ("fnCheckData8Signed", ":CheckData8Signed")
  , ("fnCheckData8Both", ":CheckData8Both")
  -- Data
  , ("opConcat", "++")
  , ("fnFill", "Fill")
  -- Integer
  , ("opPLUS", "+")
  , ("opMINUS", "-")
  , ("opMUL", "*")
  , ("opDIV", "/")
  , ("opMOD", "%")
  , ("opCOM", "~")
  , ("opAND", "&")
  , ("opOR", "|")
  , ("opXOR", "^")
  , ("opShiftL", "<<")
  , ("opShiftR", ">>")
  , ("fnRotate", "Rotate")
  , ("opSetCare", "*&")
  , ("opAddDoNotCare", "*|")
  ]
