{-# LANGUAGE TypeFamilies #-}

module Asm.Parser.Parser.Basic
  ( module Asm.Parser.Parser.Basic
  , module Export
  ) where

import           Asm.Core.Prelude
import qualified Data.Char                  as C (toUpper)
import           Data.Scientific
import           Text.Megaparsec            as Export (between, choice, getPosition, label, lookAhead, sepBy, try,
                                                       (<?>))
import qualified Text.Megaparsec            as MP
import           Text.Megaparsec.Char       as Export (newline, string, string')
import qualified Text.Megaparsec.Char       as MP
import qualified Text.Megaparsec.Char.Lexer as MP
import           Text.Megaparsec.Expr       as Export (Operator (..), makeExprParser)
import           Text.Megaparsec.Pos        as Export (SourcePos)

import           Asm.Parser.Parser.Class    as Export

--
-- global parameters
--

whiteSpaceWithNewline :: String
{-# INLINABLE whiteSpaceWithNewline #-}
whiteSpaceWithNewline = "\t\n\r\f\v "

whiteSpaceWithoutNewline :: String
{-# INLINABLE whiteSpaceWithoutNewline #-}
whiteSpaceWithoutNewline = "\t\r\f\v "

operatorChar :: String
{-# INLINABLE operatorChar #-}
operatorChar = "+-~!*/%<>=&^|"

--
-- the parser
--

-- parse whitespace (with and without newlines depending on state)
sc :: Parser ()
sc = do
  (configNewlines, _) <- get
  MP.label
    "space char"
    $ MP.hidden
    $ MP.skipMany
    $ void
    $ MP.oneOf (bool whiteSpaceWithoutNewline whiteSpaceWithNewline configNewlines)

lexeme :: Parser a -> Parser a
{-# INLINE lexeme #-}
lexeme = (<* sc)

lexeme_ :: Parser a -> Parser ()
{-# INLINE lexeme_ #-}
lexeme_ = (*> sc)

-- the initial states
initialExprState :: (Bool, Bool)
initialExprState = (True, False)

initialStmtState :: (Bool, Bool)
initialStmtState = (False, False)

-- allow newlines in the specified parser
withNewlines :: Parser a -> Parser a
{-# INLINABLE withNewlines #-}
withNewlines p = do
  oldConfig@(_, configInType) <- get
  put (True, configInType)
  sc
  r <- p
  put oldConfig
  return r

-- be "in type" in the specified parser
withInType :: Parser a -> Parser a
{-# INLINABLE withInType #-}
withInType p = do
  oldConfig@(configNewLines, _) <- get
  put (configNewLines, True)
  r <- p
  put oldConfig
  return r

-- check if we're "in type"
isInType :: Parser Bool
isInType = do
  (_, configInType) <- get
  return configInType

--
-- basic parsers
--

wrapPosition :: Parser (b -> (SourcePos, b))
{-# INLINE wrapPosition #-}
wrapPosition = (,) <$> MP.getPosition

sepBy1 :: Parser a -> Parser sep -> Parser (NonNull [a])
{-# INLINE sepBy1 #-}
sepBy1 a b = impureNonNull <$> MP.sepBy1 a b

parseDecimalInteger :: Parser Integer
{-# INLINE parseDecimalInteger #-}
parseDecimalInteger = lexeme MP.decimal

parseScientific :: Parser Scientific
{-# INLINE parseScientific #-}
parseScientific = lexeme MP.scientific

-- exact match without following chars

operator :: Text -> Parser Text
{-# INLINE operator #-}
operator name = MP.string name <* MP.notFollowedBy (MP.oneOf operatorChar)

identifier :: Text -> Parser Text
{-# INLINE identifier #-}
identifier w = MP.string w <* MP.notFollowedBy MP.alphaNumChar

identifier' :: Text -> Parser Text
{-# INLINE identifier' #-}
identifier' w = MP.string' w <* MP.notFollowedBy MP.alphaNumChar

-- identifier with optional spaces, no return value

rword :: Text -> Parser ()
{-# INLINE rword #-}
rword = lexeme_ . identifier

rword' :: Text -> Parser ()
{-# INLINE rword' #-}
rword' = lexeme_ . identifier'

-- a char or symbol

char :: Char -> Parser ()
{-# INLINE char #-}
char = void . MP.char

symbol :: Text -> Parser ()
{-# INLINE symbol #-}
symbol sym = lexeme_ $ MP.string sym

symbol' :: Text -> Parser ()
{-# INLINE symbol' #-}
symbol' sym = lexeme_ $ MP.string' sym

-- combinations of: lexeme some|many (n)oneOf(')

oneOf :: String -> Parser Char
{-# INLINE oneOf #-}
oneOf = MP.oneOf

noneOf :: String -> Parser Char
{-# INLINE noneOf #-}
noneOf = MP.noneOf

someOf :: String -> Parser Text
{-# INLINE someOf #-}
someOf = fmap pack . lexeme . some . MP.oneOf

someOf' :: String -> Parser Text
{-# INLINE someOf' #-}
someOf' = fmap pack . lexeme . some . oneOf'
  where
    -- grabbed from megaparsec 5 since it's been droppen with 6
    oneOf' cs = MP.satisfy (`elemi` cs)
    casei x y = C.toUpper x == C.toUpper y
    elemi = any . casei

manyOf :: String -> Parser Text
{-# INLINE manyOf #-}
manyOf = fmap pack . lexeme . many . MP.oneOf

someNoneOf :: String -> Parser Text
{-# INLINE someNoneOf #-}
someNoneOf = fmap pack . lexeme . some . MP.noneOf
