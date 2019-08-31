{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Asm.Cpu6809.Parser
  ( expr6809
  , asm6809
  , asmFile6809
  ) where

import           Asm.Core.Prelude
import           Data.Generics                  (extQ)
import qualified Language.Haskell.TH            as TH
import           Language.Haskell.TH.Quote      (QuasiQuoter (..), dataToExpQ)
import qualified System.IO

import           Asm.Core.Control.CompilerError
import           Asm.Core.File
import           Asm.Core.SourcePos
import           Asm.Parser.Parser
import           Asm.Parser.Quote

import           Asm.Cpu6809.Data.Cpu6809       ()
import           Asm.Cpu6809.Data.CpuData6809


asm6809 :: QuasiQuoter
asm6809 = QuasiQuoter
  { quoteExp = quoteAsm
  , quotePat = [printInternalError|QuasiQuoter "asm6809" for patterns is not available|]
  , quoteDec = [printInternalError|QuasiQuoter "asm6809" for declarations is not available|]
  , quoteType = [printInternalError|QuasiQuoter "asm6809" for types is not available|]
  }
  where
    quoteAsm :: String -> TH.ExpQ
    quoteAsm s = do
      pos <- getPosition
      expr <- parserForAsm pos s
      d <- lift6809 (expr :: PStmtBlock6809)
      return $ TH.SigE d (TH.ConT ''PStmtBlock6809)

asmFile6809 :: FilePath -> TH.ExpQ
asmFile6809 fp = do
  fp' <- getRelativeFilePathQ True fp
  s <- TH.runIO $ System.IO.readFile fp'
  expr <- parserForAsm (initialSourcePos fp') s
  d <- lift6809 (expr :: PStmtBlock6809)
  return $ TH.SigE d (TH.ConT ''PStmtBlock6809)

expr6809 :: QuasiQuoter
expr6809 = QuasiQuoter
  { quoteExp = quoteExpr
  , quotePat = [printInternalError|QuasiQuoter "expr6809" for patterns is not available|]
  , quoteDec = [printInternalError|QuasiQuoter "expr6809" for declarations is not available|]
  , quoteType = [printInternalError|QuasiQuoter "expr6809" for types is not available|]
  }
  where
    quoteExpr :: String -> TH.ExpQ
    quoteExpr s = do
      pos <- getPosition
      expr <- parserForExpr pos s
      d <- lift6809 (expr :: PExpr6809)
      return $ TH.SigE d (TH.ConT ''PExpr6809)

-- helper

lift6809 :: Data a => a -> TH.ExpQ
lift6809 = dataToExpQ (
             const Nothing
      `extQ` (antiExpr :: PExpr6809 -> Maybe TH.ExpQ)
      `extQ` (antiStmt lift6809 :: PStmt6809 -> Maybe TH.ExpQ)
      `extQ` antiParsedInt64
      `extQ` antiParsedLabelId
      `extQ` liftText
      )
