module Asm.Cpu6809.Parser.CpuStmt where

import           Asm.Core.Prelude

import           Asm.Parser.Data.PExpr
import           Asm.Parser.Parser.Expr
import           Asm.Parser.Parser.Tools

import {-# SOURCE #-} Asm.Cpu6809.Data.Cpu6809     ()
import           Asm.Cpu6809.Data.CpuData6809
import           Asm.Cpu6809.Data.OpCodes
import           Asm.Cpu6809.OpCodes.Quote

parseCpu6809Stmt :: Parser PStmtCpu6809
parseCpu6809Stmt = parseData <|> parseStmtCpu6809

parseData :: Parser PStmtCpu6809
parseData = do
  rword' "data"
  e <- parseExpr
  return $ PSData [e]

parseStmtCpu6809 :: Parser PStmtCpu6809
parseStmtCpu6809 = do
  op <-
    choice $
      map (\(s, o) -> identifier' s *> pure o) $
        sortOn (\(s, _) -> negate $ length s)
          opcodeNames
  let
    extraParsers
      | fromOperator op `elem` [ [opc|pshs.imm|], [opc|pshu.imm|], [opc|puls.imm|], [opc|pulu.imm|] ]
          = [ try (parsePshPul op) ]
      | fromOperator op `elem` [ [opc|tfr.imm|], [opc|exg.imm|] ]
          = [ try (parseTfrExg op) ]
      | otherwise = []
  choice $
    extraParsers ++
    [ try (parseInline op)
    , try (parseIndexed op)
    , parseRegular op
    ]

parsePshPul :: Asm.Cpu6809.Data.OpCodes.Operator -> Parser PStmtCpu6809
parsePshPul op = do
  _ <- optional $ string' ".imm"
  sc
  loc <- getPosition
  regs <- sepBy1 register (symbol ",")
  return $ PSRegular op [AMImm] (Just (loc, PEConstInt $ foldr (.|.) 0 regs))
  where
    register =
      choice
        [ symbol' "cc" *> pure 1
        , symbol' "a"  *> pure 2
        , symbol' "b"  *> pure 4
        , symbol' "d"  *> pure (2+4)
        , symbol' "dp" *> pure 8
        , symbol' "x"  *> pure 16
        , symbol' "y"  *> pure 32
        , symbol' "s"  *> pure 64
        , symbol' "u"  *> pure 64
        , symbol' "pc" *> pure 128
        ]

parseTfrExg :: Asm.Cpu6809.Data.OpCodes.Operator -> Parser PStmtCpu6809
parseTfrExg op = do
  _ <- optional $ string' ".imm"
  sc
  loc <- getPosition
  src <- register
  symbol ","
  dst <- register
  return $ PSRegular op [AMImm] (Just (loc, PEConstInt $ (src `shiftL` 4) .|. dst))
  where
    register =
      choice
        [ symbol' "d"  *> pure 0b0000
        , symbol' "x"  *> pure 0b0001
        , symbol' "y"  *> pure 0b0010
        , symbol' "u"  *> pure 0b0011
        , symbol' "s"  *> pure 0b0100
        , symbol' "pc" *> pure 0b0101
        , symbol' "a"  *> pure 0b1000
        , symbol' "b"  *> pure 0b1001
        , symbol' "cc" *> pure 0b1010
        , symbol' "dp" *> pure 0b1011
        ]

parseInline :: Asm.Cpu6809.Data.OpCodes.Operator -> Parser PStmtCpu6809
parseInline op = do
  (am, iw) <- char '.' *>
    choice
      [ string' "imm" *> pure (AMImm, Nothing)
      , string' "dir" *> pure (AMDir, Nothing)
      , string' "idx1" *> pure (AMIdx, Just 1)
      , string' "idx2" *> pure (AMIdx, Just 2)
      , string' "idx3" *> pure (AMIdx, Just 3)
      , string' "ext" *> pure (AMExt, Nothing)
      , string' "imp" *> pure (AMImp, Nothing)
      , string' "rel" *> pure (AMRel, Nothing)
      ]
  sc
  char '@'
  n <- parseLabelIdValue
  return $ PSInline op am iw n

parseRegular :: Asm.Cpu6809.Data.OpCodes.Operator -> Parser PStmtCpu6809
parseRegular op = do
  am <- optional $ char '.' *>
    choice
      [ string' "imm" *> pure AMImm
      , string' "dir" *> pure AMDir
      , string' "ext" *> pure AMExt
      , string' "imp" *> pure AMImp
      , string' "rel" *> pure AMRel
      ]
  sc
  e <- optional parseExpr
  return $ PSRegular op (maybe [AMImm, AMDir, AMExt, AMImp, AMRel] (:[]) am) e

parseIndexed :: Asm.Cpu6809.Data.OpCodes.Operator -> Parser PStmtCpu6809
parseIndexed op = do
  _ <- optional $ string' ".idx"
  sc
  (i, (im, e)) <- choice
    [ try $ between (symbol "[") (symbol "]") (map (\e -> (True, (IMIndirect, Just e))) parseExpr)
    , between (symbol "[") (symbol "]") (map ((,) True) go)
    , map ((,) False) go
    ]
  return $ PSIndexed op im e i
  where
    go = choice
      [ try parseOffset
      , try parseRegOffset
      ]


parseIndexRegister :: Bool -> Parser Register
parseIndexRegister withPC = choice
  [ symbol' "x" *> pure RegX
  , symbol' "y" *> pure RegY
  , symbol' "u" *> pure RegU
  , symbol' "s" *> pure RegS
  , bool mzero (rword' "pc" *> pure RegPC) withPC
  ]

parseOffset :: Parser (IndexedMode, Maybe PExpr6809)
parseOffset = do
  e <- optional parseExpr
  isRel <-
    ((symbol ",," <|> symbol ";") *> pure True) <|>
    (symbol "," *> pure False)
  dec <- optional $ (symbol' "--" *> pure True) <|> (symbol' "-" *> pure False)
  r <- parseIndexRegister True
  inc <- optional $ (symbol' "++" *> pure True) <|> (symbol' "+" *> pure False)
  case (dec, inc, isRel /= (r == RegPC)) of
    (Nothing, Nothing, False) ->
      return (IMOffset r, e)
    (Just dec', Nothing, False) ->
      return (IMIncrement r True dec', e)
    (Nothing, Just inc', False) ->
      return (IMIncrement r False inc', e)
    (Nothing, Nothing, True) ->
      if isJust e
        then return (IMRelOffset r, e)
        else mzero
    _ ->
      mzero

parseRegOffset :: Parser (IndexedMode, Maybe PExpr6809)
parseRegOffset = do
  e <- choice
    [ symbol' "a" *> pure RegA
    , symbol' "b" *> pure RegB
    , symbol' "d" *> pure RegD
    ]
  symbol ","
  r <- parseIndexRegister False
  return (IMRegOffset r e, Nothing)
