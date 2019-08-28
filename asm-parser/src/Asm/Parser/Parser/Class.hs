{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Asm.Parser.Parser.Class
  ( Parser
  , CpuParser(..)
  ) where

import           Asm.Core.Prelude
import           Control.Monad.State.Strict    (StateT)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Pos           as Export (SourcePos)

import           Asm.Core.Data.Cpu
import           Asm.Core.Data.CpuData

import           Asm.Parser.ToCompiler.PCState

--
-- the parser
--

type Parser = StateT (Bool, Bool) (Parsec Void Text)

class (Show pe, Show ps, Cpu c) => CpuParser c ps pe | c -> ps, ps -> pe, pe -> c where
  parseCpuExpr :: Parser pe
  parseCpuStmt :: Parser ps
  convertCpuExpr :: [SourcePos] -> pe -> PCSM (CE12 c)
  convertCpuStmt :: ps -> PCSM (CS12 c)
