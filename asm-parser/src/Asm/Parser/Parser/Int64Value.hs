{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Asm.Parser.Parser.Int64Value
  ( Int64Value(..)
  , int64Value
  , parseInt64Value
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Control.CompilerError
import           Asm.Core.SourcePos

import           Asm.Parser.Data.Int64Value
import           Asm.Parser.Parser.Basic
import           Asm.Parser.Parser.Haskell
import           Asm.Parser.Parser.Integer

-- extract

int64Value :: Location -> Int64Value -> Int64
int64Value _  (Int64Value x)       =  x
int64Value loc Int64ValueHaskell{} = $printError [(loc, "parsed string still not executed")]

-- parse

parseInt64Value :: Parser Int64Value
parseInt64Value = choice
  [ Int64ValueHaskell <$> (char '$' *> parseHaskellExpr)
  , Int64Value . fromIntegral <$> parseInteger
  ]
