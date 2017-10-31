module Asm.Parser.Parser.Haskell
  ( parseHaskellExpr
  , parseHaskellTermParens
  , parseHaskellTermNewline
  ) where

import           Asm.Core.Prelude
import           Language.Haskell.Exts.Extension
import           Language.Haskell.Exts.Parser           (ParseMode (..))
import qualified Language.Haskell.Exts.Parser           as Hs
import qualified Language.Haskell.Exts.SrcLoc           as Hs
import qualified Language.Haskell.Exts.Syntax           as Hs
import qualified Language.Haskell.Meta.Parse            as Hs
import qualified Language.Haskell.Meta.Syntax.Translate as Hs
import           Language.Haskell.TH                    (Exp)
import qualified Language.Haskell.TH                    as TH
import qualified Text.Megaparsec                        as MP
import qualified Text.Megaparsec.Char                   as MP

import           Asm.Parser.Parser.Basic


-- Haskell entry

parseHaskellExpr :: Parser Exp
parseHaskellExpr = lexeme $ choice
  [ TH.VarE . TH.mkName <$> haskellVarName
  , TH.ConE . TH.mkName <$> haskellConName
  , parseHaskellTermParens '(' ')'
  ]

-- haskell sub-parsers

haskellVarName :: Parser String
haskellVarName = (:) <$> oneOf "abcdefghijklmnopqrstuvwxyz" <*> many (MP.oneOf haskellNameChar)

haskellConName :: Parser String
haskellConName = (:) <$> oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ" <*> many (MP.oneOf haskellNameChar)

haskellNameChar :: String
{-# INLINABLE haskellNameChar #-}
haskellNameChar = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'"

parseHaskellTermParens :: Char -> Char -> Parser Exp
parseHaskellTermParens op cl = do
  char op
  (code, ()) <- MP.match helper
  char cl
  either
    fail
    return
    (parseExp (unpack code))
  where
    helper :: Parser ()
    helper =
      void $
        many $
          choice
            [ void $ some $ noneOf (op:cl:"\"")
            , char '\\' <* MP.anyChar
            , do
                char '"'
                void $ many
                  (
                    noneOf "\\\"" <|> ( char '\\' *> MP.anyChar )
                  )
                char '"'
            , do
                char op
                helper
                char cl
            ]

parseHaskellTermNewline :: Parser Exp
parseHaskellTermNewline = do
  code <- many (noneOf "\n")
  either
    fail
    return
    (parseExp code)

-- adapted from Language.Haskell.Meta.Parse

parseExp :: String -> Either String Exp
parseExp = map Hs.toExp . parseHsExp

parseHsExp :: String -> Either String (Hs.Exp Hs.SrcSpanInfo)
parseHsExp = Hs.parseResultToEither . Hs.parseExpWithMode inlineParseMode

inlineParseMode :: ParseMode
inlineParseMode = ParseMode
  { parseFilename = []
  , baseLanguage = Haskell2010
  , extensions =
      map
        EnableExtension
        [ BinaryLiterals
        ]
  , ignoreLinePragmas = False
  , ignoreLanguagePragmas = False
  , fixities = Nothing
  , ignoreFunctionArity = False
  }
