{-# LANGUAGE NoImplicitPrelude #-}

module Asm.Parser.Parser
  ( parserForExpr
  , parserForAsm
  ) where

import           Asm.Core.Prelude

import           Control.Monad.Fail         (MonadFail)
import           Control.Monad.State.Strict (runStateT)
import           Text.Megaparsec            (ParseErrorBundle, Parsec, PosState (..), State (..), defaultTabWidth, eof,
                                             errorBundlePretty, runParser')

import           Asm.Parser.BuildTree
import           Asm.Parser.Data.PExpr
import           Asm.Parser.Data.PStmt
import           Asm.Parser.Parser.Expr
import           Asm.Parser.Parser.Stmt
import           Asm.Parser.Parser.Tools
import           Asm.Parser.StripComments

parserForExpr :: (CpuParser c ps pe, MonadFail m) => SourcePos -> String -> m (PExpr pe)
parserForExpr pos s =
  case runParser'' (runStateT p initialExprState) pos (stripComments s) of
    Left err     -> fail $ errorBundlePretty err
    Right (e, _) -> return e
  where
    p = do
      sc
      e <- parseExpr
      eof
      return e

parserForAsm :: (CpuParser c ps pe, MonadFail m) => SourcePos -> String -> m [PStmt ps pe]
parserForAsm pos s =
  case runParser'' (runStateT p initialStmtState) pos (stripComments s) of
    Left err     -> fail $ errorBundlePretty err
    Right (e, _) -> buildTree (filter (not . isPSNothing) e)
  where
    p = do
      sc
      e <- parseAsm
      eof
      return e

runParser''
  :: Parsec e s a -- ^ Parser to run
  -> SourcePos    -- ^ Initial state
  -> s
  -> Either (ParseErrorBundle s e) a
runParser'' p pos s =
  snd $
    runParser'
    p
    State
    { stateInput  = s
    , stateOffset = 0
    , statePosState = PosState
      { pstateInput = s
      , pstateOffset = 0
      , pstateSourcePos = pos
      , pstateTabWidth = defaultTabWidth
      , pstateLinePrefix = ""
      }
    }
