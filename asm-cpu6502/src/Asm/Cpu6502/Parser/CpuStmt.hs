module Asm.Cpu6502.Parser.CpuStmt
  ( parseCpu6502Stmt
  ) where

import           Asm.Core.Prelude

import           Asm.Parser.Parser.Expr
import           Asm.Parser.Parser.Tools

import {-# SOURCE #-} Asm.Cpu6502.Data.Cpu6502     ()
import           Asm.Cpu6502.Data.CpuData6502
import           Asm.Cpu6502.Data.OpCodes

parseCpu6502Stmt :: Parser PStmtCpu6502
parseCpu6502Stmt = parseData <|> parseStmtCpu6502

parseData :: Parser PStmtCpu6502
parseData = do
  rword' "data"
  e <- parseExpr
  return $ PSData [e]

parseStmtCpu6502 :: Parser PStmtCpu6502
parseStmtCpu6502 = do
  op <-
    choice $
      map (\(s, o) -> identifier' s *> pure o) $
        sortOn (\(s, _) -> negate $ length s)
          opcodeNames
  choice
    [ try (parseInline op)
    , try (parseRegular op)
    ]

parseInline :: Asm.Cpu6502.Data.OpCodes.Operator -> Parser PStmtCpu6502
parseInline op = do
  am <- char '.' *>
    choice
      [ string' "abs" *> pure AMAbs
      , string' "zp" *> pure AMZp
      , string' "imm" *> pure AMImm
      , string' "rel" *> pure AMRel
      ]
  sc
  (im, name) <- parseIndex $ char '@' *> parseLabelIdValue
  return $ PSInline op im am name

parseRegular :: Asm.Cpu6502.Data.OpCodes.Operator -> Parser PStmtCpu6502
parseRegular op = try $ do
  am <- optional (
    char '.' *> choice
      [ string' "abs" *> pure AMAbs
      , string' "zp" *> pure AMZp
      , string' "imm" *> pure AMImm
      , string' "imp" *> pure AMImp
      , string' "rel" *> pure AMRel
      ]
    )
  sc
  (im, exprMay) <- (map Just <$> parseIndex parseExpr) <|> pure (IMNone, Nothing)
  return $ PSRegular op im (maybe [AMImm, AMZp, AMAbs, AMImp, AMRel] (:[]) am) exprMay

parseIndex :: Parser a -> Parser (IndexMode, a)
parseIndex p =
  choice
    [ try $ parseIdxAddrMode p
    , try $ parseIndYAddrMode p
    , try $ parseXIndAddrMode p
    , try $ parseIndAddrMode p
    , try $ parseDirAddrMode p
    ]

parseDirAddrMode :: Parser a -> Parser (IndexMode, a)
parseDirAddrMode p = do
  expr <- p
  return (IMNone, expr)

parseIdxAddrMode :: Parser a -> Parser (IndexMode, a)
parseIdxAddrMode p = do
  expr <- p
  symbol ","
  im <- choice
    [ symbol' "x" *> pure IMX
    , symbol' "y" *> pure IMY
    ]
  return (im, expr)

parseIndYAddrMode :: Parser a -> Parser (IndexMode, a)
parseIndYAddrMode p = do
  symbol "["
  expr <- p
  symbol "]"
  symbol ","
  symbol' "y"
  return (IMIY, expr)

parseXIndAddrMode :: Parser a -> Parser (IndexMode, a)
parseXIndAddrMode p = do
  symbol "["
  expr <- p
  symbol ","
  symbol' "x"
  symbol "]"
  return (IMXI, expr)

parseIndAddrMode :: Parser a -> Parser (IndexMode, a)
parseIndAddrMode p = do
  symbol "["
  expr <- p
  symbol "]"
  return (IMI, expr)
