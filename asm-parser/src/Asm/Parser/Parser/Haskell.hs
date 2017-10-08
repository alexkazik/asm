module Asm.Parser.Parser.Haskell
  ( parseHaskellExpr
  , parseHaskellTerm
  ) where

import           Asm.Core.Prelude
import qualified Text.Megaparsec.Char       as MP
import qualified Text.Megaparsec.Char.Lexer as MP

import           Asm.Parser.Data.Haskell
import           Asm.Parser.Parser.Basic
import           Asm.Parser.Parser.Integer


-- Haskell entry

parseHaskellExpr :: Parser Haskell
parseHaskellExpr = lexeme $ choice
  [ HVar <$> haskellVarName
  , HCon <$> haskellConName
  , between (symbol "(") (symbol ")") (withNewlines parseHaskellTerm)
  ]

-- haskell sub-parsers

haskellVarName :: Parser String
haskellVarName = (:) <$> oneOf "abcdefghijklmnopqrstuvwxyz" <*> (many $ MP.oneOf haskellNameChar)

haskellConName :: Parser String
haskellConName = (:) <$> oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ" <*> (many $ MP.oneOf haskellNameChar)

haskellNameChar :: String
{-# INLINABLE haskellNameChar #-}
haskellNameChar = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'"

parseHaskellTerm :: Parser Haskell
parseHaskellTerm = do
  t <- try (do
      l <- parseHaskellApp
      o <- someOf operatorChar
      r <- parseHaskellApp
      return $ HApp $ impureNonNull [HVar (unpack o), l, r]
    ) <|> parseHaskellApp

  optional (symbol "::" *> haskellConName) >>= \case
    Nothing -> return t
    Just ty -> return $ HSig ty t

parseHaskellApp :: Parser Haskell
parseHaskellApp = do
  a <- sepBy1 parseHaskellElem (some $ MP.oneOf whiteSpaceWithoutNewline)
  return $ HApp a

parseHaskellElem :: Parser Haskell
parseHaskellElem = choice
  [ HVar <$> haskellVarName
  , HCon <$> haskellConName
  , between (symbol "(") (symbol ")") parseHaskellTerm
  , HString <$> (char '"' *> many (noneOf "\\\"" <|> (char '\\' *> oneOf "\\\"")) <* char '"')
  , try (HInteger <$> parseInteger)
  , HScientific <$> MP.scientific
  ]
