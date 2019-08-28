module Asm.Parser.Parser.Stmt
  ( parseAsm
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Data.VariableType

import           Asm.Parser.Data.PExpr
import           Asm.Parser.Data.PStmt
import           Asm.Parser.Parser.Expr
import           Asm.Parser.Parser.Tools

parseAsm :: CpuParser c ps pe => Parser [PStmt ps pe]
parseAsm = sepBy stmt (symbol "\n" <|> symbol ":")

stmt :: CpuParser c ps pe => Parser (PStmt ps pe)
stmt = wrapPosition <*> choice
  [ try $ PSCpuStmt <$> parseCpuStmt
  , rword' "if" *> (PSBuildIf <$> parseExpr)
  , rword' "elseif" *> (PSBuildElseif <$> parseExpr)
  , rword' "else" *> pure PSBuildElse
  , rword' "endif" *> pure PSBuildEndif
  , rword' "$if" *> (PSBuildDirectIf <$> parseHaskellTermNewline)
  , rword' "$elseif" *> (PSBuildDirectElseif <$> parseHaskellTermNewline)
  , rword' "$else" *> pure PSBuildDirectElse
  , rword' "$endif" *> pure PSBuildDirectEndif
  , rword' "begin" *> pure (PSBuildNamespace Nothing)
  , rword' "end" *> pure PSBuildEnd
  , symbol "{" *> pure (PSBuildNamespace Nothing)
  , symbol "}" *> pure PSBuildEnd
  , try ((PSLabelDefinition <$> parseLabelIdValue) <* lookAhead (symbol ":"))
  , parseNamespace
  , parseBlock
  , parseFor
  , parseAntiNamespace
  , parseAntiBlock
  , parseQuickAntiNamespace
  , parseVariable
  , parsePool
  , parseTypeDefStmt
  , parseAlias
  , parseMetaSet
  , parseMeta
  , sc *> pure PSNothing
  ]

parseNamespace :: Parser (PStmtI ps pe)
parseNamespace = do
  rword' "namespace"
  name <- optional parseLabelIdValue
  return (PSBuildNamespace name)

parseBlock :: CpuParser c ps pe => Parser (PStmtI ps pe)
parseBlock = do
  rword' "block"
  name <- optional parseLabelIdValue
  pool <- optional $ symbol "@" *> parseExpr
  return (PSBuildBlock name pool)

parseFor :: CpuParser c ps pe => Parser (PStmtI ps pe)
parseFor = do
  rword' "for"
  name <- parseLabelIdValue
  symbol "="
  from <- parseExprTerm
  cmp <- (symbol "<" *> pure False) <|> (symbol "<=" *> pure True)
  to <- parseExprTerm
  loc <- getSourcePos
  step <- optional (symbol "+=" *> parseExprTerm)
  return (PSBuildFor name from cmp to (fromMaybe (loc, PEConstInt 1) step))

parseAntiNamespace :: Parser (PStmtI ps pe)
parseAntiNamespace = do
  rword' "$namespace"
  va <- parseHaskellExpr
  name <- optional parseLabelIdValue
  return (PSAntiNamespace va name)

parseAntiBlock :: CpuParser c ps pe => Parser (PStmtI ps pe)
parseAntiBlock = do
  rword' "$block"
  va <- parseHaskellExpr
  name <- optional parseLabelIdValue
  pool <- optional $ symbol "@" *> parseExpr
  return (PSAntiBlock va name pool)

parseQuickAntiNamespace :: Parser (PStmtI ps pe)
parseQuickAntiNamespace = do
  symbol "$"
  lookAhead $ symbol "("
  va <- parseHaskellExpr
  name <- optional parseLabelIdValue
  return (PSAntiNamespace va name)

parseMetaSet :: CpuParser c ps pe => Parser (PStmtI ps pe)
parseMetaSet = do
  rword' "set"
  key <- parseExpr
  symbol "="
  value <- parseExpr
  return $ PSMetaSet key value

parseMeta :: CpuParser c ps pe => Parser (PStmtI ps pe)
parseMeta = do
  mode <- choice
    [ pure PSMetaUnset <* rword' "unset"
    , pure PSMetaSticky <* rword' "sticky"
    , pure PSMetaUnsticky <* rword' "unsticky"
    ]
  key <- parseExpr
  return $ mode key

parseVariable :: CpuParser c ps pe => Parser (PStmtI ps pe)
parseVariable = do
  vt <- choice
    [ rword' "var" *> pure VTVar
    , rword' "const" *> pure VTConst
    , rword' "local" *> pure VTLocal
    , rword' "inline" *> pure VTInline
    , rword' "pointer" *> pure VTPointer
    ]
  typ <- withInType parseExpr
  name <- parseLabelIdValue
  value <- optional $ symbol "=" *> parseExpr
  pool <- optional $ symbol "@" *> parseExpr
  align <- optional $ rword' "align" *> parseInt64Value
  page <- optional $ rword' "page" *> parseInt64Value
  return $ PSVariable vt name typ value pool (fromMaybe (Int64Value 1) align) page

parseTypeDefStmt :: CpuParser c ps pe => Parser (PStmtI ps pe)
parseTypeDefStmt = do
  rword' "type"
  typ <- withInType parseExpr
  name <- parseLabelIdValue
  return $ PSTypeDef name typ

parsePool :: Parser (PStmtI ps pe)
parsePool = do
  rword' "pool"
  na <- parseLabelIdValue
  sub <- optional $ between (symbol "[") (symbol "]") $ sepBy parseLabelIdValue (symbol ",")
  symbol "="
  virt <- optional $ symbol' "virtual"
  value <- parseInt64Value
  bank <- optional $ symbol "@" *> parseInt64Value
  return $ PSPoolStmt na (fromMaybe [] sub) (isJust virt) value (fromMaybe (Int64Value 0) bank)

parseAlias :: CpuParser c ps pe => Parser (PStmtI ps pe)
parseAlias= do
  rword' "alias"
  na <- parseLabelIdValue
  symbol "="
  value <- parseExpr
  return $ PSAlias na value
