{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Asm.Cpu6502.Suffixed
  ( module Asm.Core
  , module Asm.Parser
  , Asm6502
  , asm6502
  , asmFile6502
  , Expr6502
  , expr6502
  , ToByte6502
  , byte6502
  , ToArray6502
  , array6502
  , ToStructOrUnion6502
  , structOrUnion6502
  , struct6502
  , union6502
  , compile6502
  ) where

import           Asm.Core.Prelude
import qualified Data.Map.Strict                 as M
import qualified Language.Haskell.TH             as TH
import           Language.Haskell.TH.Quote       (dataToExpQ)

import           Asm.Core
import           Asm.Core.SourcePos
import           Asm.Parser
import           Asm.Parser.Data.PExpr
import           Asm.Parser.Data.ToArray
import           Asm.Parser.Data.ToByte
import           Asm.Parser.Data.ToExpr
import           Asm.Parser.Data.ToStructOrUnion

import           Asm.Cpu6502.Data.CpuData6502
import           Asm.Cpu6502.Data.CpuSource6502  ()
import           Asm.Cpu6502.Parser

-- external names

type Asm6502 = PStmtBlock6502
type Expr6502 = PExpr6502

-- ToByte specialized to this cpu

type ToByte6502 e = ToByte PExprCpu6502 e

toByte6502 :: ToByte6502 a => SourcePos -> a -> Expr6502
toByte6502 = toByte

byte6502 :: TH.Q TH.Exp
byte6502 = do
  loc <- getPosition
  loc' <- dataToExpQ (const Nothing) loc
  return $ TH.VarE 'toByte6502 `TH.AppE` loc'

-- ToArray specialized to this cpu

type ToArray6502 e = ToArray PExprCpu6502 e

toArray6502 :: ToArray6502 a => SourcePos -> a -> Expr6502
toArray6502 = toArray

array6502 :: TH.Q TH.Exp
array6502 = do
  loc <- getPosition
  loc' <- dataToExpQ (const Nothing) loc
  return $ TH.VarE 'toArray6502 `TH.AppE` loc'

-- ToStructOrUnion specialized to this cpu

type ToStructOrUnion6502 e = ToStructOrUnion PExprCpu6502 e

toStructOrUnion6502 :: ToStructOrUnion6502 a => SourcePos -> a -> Expr6502
toStructOrUnion6502 = toStructOrUnion

structOrUnion6502 :: TH.Q TH.Exp
structOrUnion6502 = do
  loc <- getPosition
  loc' <- dataToExpQ (const Nothing) loc
  return $ TH.VarE 'toStructOrUnion6502 `TH.AppE` loc'

-- specialized versions of structOrUnion6502

struct6502 :: TH.Q TH.Exp
struct6502 = do
  loc <- getPosition
  loc' <- dataToExpQ (const Nothing) loc
  t <- [t|[(Text, Expr6502)] -> Expr6502|]
  return $
    TH.SigE
      (TH.VarE 'toStructOrUnion6502 `TH.AppE` loc')
      t

toUnion6502 :: ToExpr PExprCpu6502 a => SourcePos -> Text -> a -> Expr6502
toUnion6502 loc k v = (loc, PEUserStructOrUnion (M.singleton k (toExpr loc v)))

union6502 :: TH.Q TH.Exp
union6502 = do
  loc <- getPosition
  loc' <- dataToExpQ (const Nothing) loc
  return $ TH.VarE 'toUnion6502 `TH.AppE` loc'

-- the compile function

compile6502 :: Asm6502 -> CompilerResult Cpu6502
compile6502 = compileGeneric'
