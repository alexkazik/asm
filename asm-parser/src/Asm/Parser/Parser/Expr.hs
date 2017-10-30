module Asm.Parser.Parser.Expr
  ( parseExpr
  , parseExprTerm
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Data.ByteVal

import           Asm.Parser.Data.PExpr
import           Asm.Parser.Parser.Integer
import           Asm.Parser.Parser.Tools

parseExpr :: CpuParser c ps pe => Parser (PExpr pe)
parseExpr = makeExprParser parseExprTerm aOperators

-- operator

aOperators :: [[Operator Parser (PExpr pe)]]
aOperators =
  [ [ Prefix (operator1 "+")
    , Prefix (operator1 "-")
    , Prefix (operator1 "!")
    , Prefix (operator1 "~") ]
  , [ InfixL (operator2 "*")
    , InfixL (operator2 "/")
    , InfixL (operator2 "/+")
    , InfixL (operator2 "%") ]
  , [ InfixL (operator2 "+")
    , InfixL (operator2 "-") ]
  , [ InfixL (operator2 "++") ]
  , [ InfixL (operator2 "<<")
    , InfixL (operator2 ">>")
    , InfixL (operator2 ">>+") ]
  , [ InfixL (operator2 "<")
    , InfixL (operator2 "<=")
    , InfixL (operator2 ">")
    , InfixL (operator2 ">=") ]
  , [ InfixL (operator2 "==")
    , InfixL (operator2 "!=") ]
  , [ InfixL (operator2 "&") ]
  , [ InfixL (operator2 "^") ]
  , [ InfixL (operator2 "|") ]
  , [ InfixL (operator2 "*&")
    , InfixL (operator2 "*|") ]
  , [ InfixL (operator2 "&&") ]
  , [ InfixL (operator2 "||") ]
  ]

operator1 :: Text -> Parser (PExpr pe -> PExpr pe)
operator1 name = do
  loc <- getPosition
  op <- try $ lexeme $ operator name
  return (\a -> (loc, PEUOperator op a))

operator2 :: Text -> Parser (PExpr pe -> PExpr pe -> PExpr pe)
operator2 name = do
  loc <- getPosition
  op <- try $ lexeme $ operator name
  return (\a b -> (loc, PEBOperator op a b))

-- term

parseExprTerm :: CpuParser c ps pe => Parser (PExpr pe)
parseExprTerm = do
  expr <- aTerm'
  dref <- many deref
  return $ foldl' (flip ($)) expr dref
  where
    deref :: CpuParser c ps pe => Parser (PExpr pe -> PExpr pe)
    deref = derefStruct <|> (bool derefArray defineArray =<< isInType)
    derefStruct :: CpuParser c ps pe => Parser (PExpr pe -> PExpr pe)
    derefStruct = do
      loc <- getPosition
      symbol "."
      name <- parseLabelIdValue
      return (\x -> (loc, PEDerefStruct x name))
    derefArray :: CpuParser c ps pe => Parser (PExpr pe -> PExpr pe)
    derefArray = do
      loc <- getPosition
      symbol "["
      expr <- parseExpr
      symbol "]"
      return (\x -> (loc, PEDerefArray x expr))
    defineArray :: CpuParser c ps pe => Parser (PExpr pe -> PExpr pe)
    defineArray = do
      (loc, expr) <- optionalExprInBrackets
      return (\x -> (loc, PEDefineArray x expr))


aTerm' :: CpuParser c ps pe => Parser (PExpr pe)
aTerm' = choice
  [ between (symbol "(") (symbol ")") parseExpr
  , wrapPosition <*> (PECpuExpr <$> parseCpuExpr)
  , wrapPosition <*> (PEMagicValue <$> (char '#' *> someOf' "abcdefghijklmnopqrstuvwxyz0123456789"))
  , wrapPosition <*> (rword' "true"  *> pure (PEConstBool True))
  , wrapPosition <*> (rword' "false" *> pure (PEConstBool False))
  , wrapPosition <*> (rword' "unused" *> pure (PEByteVal ByteValAny))
  , parseStructOrUnion
  , try function
  , var
  , wrapPosition <*> parseIntExprI
  , try antiArray
  , try antiStruct
  , antitovar
  , array
  ]

parseStructOrUnion :: CpuParser c ps pe => Parser (PExpr pe)
parseStructOrUnion = do
  loc <- getPosition
  (td, isStruct) <- choice
    [ rword' "struct" *> pure (PETypeStruct, True)
    , rword' "union" *> pure (PETypeUnion, False)
    ]
  symbol "{"
  d <- withNewlines $ catMaybes <$> sepBy (try (Just <$> parseTypeName isStruct) <|> (sc *> pure Nothing)) (symbol ";")
  symbol "}"
  arr <- optional $ do
    (aloc, expr) <- optionalExprInBrackets
    return (aloc, fromMaybe (loc, PEConstInt 0) expr)
  case arr of
    Nothing         -> return (loc, td d)
    Just (aloc,len) -> return (aloc, PEDerefArray (loc, td d) len)

function :: CpuParser c ps pe => Parser (PExpr pe)
function = do
  loc <- getPosition
  name <- parseLabelIdValue
  params <- between (symbol "(") (symbol ")") $ sepBy parseExpr (symbol ",")
  return (loc, PEFunction name params)

var :: CpuParser c ps pe => Parser (PExpr pe)
var = do
  loc <- getPosition
  name <- parseLabelId
  -- important: this parser can never parse a Haskell variable, but antitovar is used
  return (loc, PELabelId name)

antiArray :: CpuParser c ps pe => Parser (PExpr pe)
antiArray = do
  loc <- getPosition
  char '$'
  lookAhead (char '[')
  expr <- parseHaskellTermParens '[' ']'
  sc
  return (loc, PEAntiArray expr)

antiStruct :: CpuParser c ps pe => Parser (PExpr pe)
antiStruct = do
  loc <- getPosition
  char '$'
  lookAhead (char '{')
  expr <- parseHaskellTermParens '{' '}'
  sc
  return (loc, PEAntiStruct expr)

antitovar :: CpuParser c ps pe => Parser (PExpr pe)
antitovar = do
  loc <- getPosition
  char '$'
  expr <- parseHaskellExpr
  return (loc, PEAntiExpr expr)

array :: CpuParser c ps pe => Parser (PExpr pe)
array = do
  loc <- getPosition
  symbol "[["
  d <- withNewlines $ catMaybes <$> sepBy (try (Just <$> parseExpr) <|> (sc *> pure Nothing)) (symbol ",")
  symbol "]]"
  return (loc, PEUserArrayL d)

-- helpers

parseTypeName :: CpuParser c ps pe => Bool -> Parser (Maybe LabelIdValue, PExpr pe)
parseTypeName isStruct = do
  t <- withInType parseExpr
  n <-
    case (isStruct, snd t) of
      (False, PETypeStruct{}) -> optional parseLabelIdValue
      (True, PETypeUnion{})   -> optional parseLabelIdValue
      _                       -> Just <$> parseLabelIdValue
  return (n, t)

optionalExprInBrackets :: CpuParser c ps pe => Parser (SourcePos, Maybe (PExpr pe))
optionalExprInBrackets = do
  loc <- getPosition
  symbol "["
  expr <- optional parseExpr
  symbol "]"
  return (loc, expr)
