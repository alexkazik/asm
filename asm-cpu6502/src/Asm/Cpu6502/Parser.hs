{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Asm.Cpu6502.Parser
  ( expr6502
  , asm6502
  , asmFile6502
  ) where

import           Asm.Core.Prelude
import           Data.Generics
import qualified Language.Haskell.TH            as TH
import           Language.Haskell.TH.Quote
import qualified System.IO

import           Asm.Core.Control.CompilerError
import           Asm.Core.File
import           Asm.Core.SourcePos
import           Asm.Parser.Parser
import           Asm.Parser.Quote

import           Asm.Cpu6502.Data.Cpu6502       ()
import           Asm.Cpu6502.Data.CpuData6502


asm6502 :: QuasiQuoter
asm6502 = QuasiQuoter
  { quoteExp = quoteAsm
  , quotePat = [printInternalError|QuasiQuoter "asm6502" for patterns is not available|]
  , quoteDec = [printInternalError|QuasiQuoter "asm6502" for declarations is not available|]
  , quoteType = [printInternalError|QuasiQuoter "asm6502" for types is not available|]
  }
  where
    quoteAsm :: String -> TH.ExpQ
    quoteAsm s = do
      pos <- getPosition
      expr <- parserForAsm pos s
      d <- lift6502 (expr :: PStmtBlock6502)
      return $ TH.SigE d (TH.ConT ''PStmtBlock6502)

asmFile6502 :: FilePath -> TH.ExpQ
asmFile6502 fp = do
  fp' <- getRelativeFilePathQ True fp
  s <- TH.runIO $ System.IO.readFile fp'
  expr <- parserForAsm (initialSourcePos fp') s
  d <- lift6502 (expr :: PStmtBlock6502)
  return $ TH.SigE d (TH.ConT ''PStmtBlock6502)

expr6502 :: QuasiQuoter
expr6502 = QuasiQuoter
  { quoteExp = quoteExpr
  , quotePat = [printInternalError|QuasiQuoter "expr6502" for patterns is not available|]
  , quoteDec = [printInternalError|QuasiQuoter "expr6502" for declarations is not available|]
  , quoteType = [printInternalError|QuasiQuoter "expr6502" for types is not available|]
  }
  where
    quoteExpr :: String -> TH.ExpQ
    quoteExpr s = do
      pos <- getPosition
      expr <- parserForExpr pos s
      d <- lift6502 (expr :: PExpr6502)
      return $ TH.SigE d (TH.ConT ''PExpr6502)

-- helper

lift6502 :: Data a => a -> TH.ExpQ
lift6502 = dataToExpQ (
             const Nothing
      `extQ` (antiExpr :: PExpr6502 -> Maybe TH.ExpQ)
      `extQ` (antiStmt lift6502 :: PStmt6502 -> Maybe TH.ExpQ)
      `extQ` antiParsedInt64
      `extQ` antiParsedLabelId
      `extQ` liftText
      )
