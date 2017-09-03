module Asm.Parser.Parser
  ( parserForExpr
  , parserForAsm
  ) where

import           Asm.Core.Prelude

import           Control.Monad.Fail         (MonadFail)
import           Control.Monad.State.Strict (runStateT)
import           Text.Megaparsec

import           Asm.Parser.BuildTree
import           Asm.Parser.Data.PExpr
import           Asm.Parser.Data.PStmt
import           Asm.Parser.Parser.Expr
import           Asm.Parser.Parser.Stmt
import           Asm.Parser.Parser.Tools
import           Asm.Parser.StripComments

parserForExpr :: (CpuParser c ps pe, MonadFail m) => SourcePos -> String -> m (PExpr pe)
parserForExpr pos s =
  case runParser (runStateT p initialExprState) "" (stripComments s) of
    Left err     -> fail $ parseErrorPretty err
    Right (e, _) -> return e
  where
    p = do
      setPosition pos
      sc
      e <- parseExpr
      eof
      return e

parserForAsm :: (CpuParser c ps pe, MonadFail m) => SourcePos -> String -> m [PStmt ps pe]
parserForAsm pos s =
  case runParser (runStateT p initialStmtState) "" (stripComments s) of
    Left err     -> fail $ parseErrorPretty err
    Right (e, _) -> buildTree (filter (not . isPSNothing) e)
  where
    p = do
      setPosition pos
      sc
      e <- parseAsm
      eof
      return e
