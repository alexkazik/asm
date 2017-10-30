module Asm.Parser.Parser.LabelIdValue
  ( LabelIdValue(..)
  , labelIdValue
  , parseLabelId
  , parseLabelIdValue
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Control.CompilerError
import           Asm.Core.SourcePos

import           Asm.Parser.Data.LabelIdValue
import           Asm.Parser.Parser.Basic
import           Asm.Parser.Parser.Haskell

-- extract

labelIdValue :: Location -> LabelIdValue -> Text
labelIdValue _ (LabelIdValue x@(a:as))
  | a `elem` assemblerIdentifierFirst && all (`elem` assemblerIdentifier) as = pack x
labelIdValue loc (LabelIdValue x) = $printError [(loc, "passed string contains invalid chars, got \"" ++ x ++ "\"")]
labelIdValue loc LabelIdValueHaskell{} = $printError [(loc, "parsed string still not executed")]

-- parse

parseLabelIdValue :: Parser LabelIdValue
parseLabelIdValue = choice
  [ LabelIdValueHaskell <$> (char '$' *> parseHaskellExpr)
  , parseLabelId
  ]

parseLabelId :: Parser LabelIdValue
parseLabelId = LabelIdValue <$> ((:) <$> oneOf assemblerIdentifierFirst <*> (unpack <$> manyOf assemblerIdentifier))

-- internals

assemblerIdentifierFirst :: String
{-# INLINABLE assemblerIdentifierFirst #-}
assemblerIdentifierFirst = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

assemblerIdentifier :: String
{-# INLINABLE assemblerIdentifier #-}
assemblerIdentifier = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
