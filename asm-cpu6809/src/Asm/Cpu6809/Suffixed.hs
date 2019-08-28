{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Asm.Cpu6809.Suffixed
  ( module Asm.Core
  , module Asm.Parser
  , Asm6809
  , asm6809
  , asmFile6809
  , Expr6809
  , expr6809
  , ToByte6809
  , byte6809
  , ToArray6809
  , array6809
  , ToStructOrUnion6809
  , structOrUnion6809
  , struct6809
  , union6809
  , compile6809
  ) where

import           Asm.Core.Prelude
import qualified Data.Map.Strict                 as M
import qualified Language.Haskell.TH             as TH
import           Language.Haskell.TH.Quote

import           Asm.Core
import           Asm.Core.SourcePos
import           Asm.Parser
import           Asm.Parser.Data.PExpr
import           Asm.Parser.Data.ToArray
import           Asm.Parser.Data.ToByte
import           Asm.Parser.Data.ToExpr
import           Asm.Parser.Data.ToStructOrUnion

import           Asm.Cpu6809.Data.CpuData6809
import           Asm.Cpu6809.Data.CpuSource6809  ()
import           Asm.Cpu6809.Parser

-- external names

type Asm6809 = PStmtBlock6809
type Expr6809 = PExpr6809

-- ToByte specialized to this cpu

type ToByte6809 e = ToByte PExprCpu6809 e

toByte6809 :: ToByte6809 a => SourcePos -> a -> Expr6809
toByte6809 = toByte

byte6809 :: TH.Q TH.Exp
byte6809 = do
  loc <- getPosition
  loc' <- dataToExpQ (const Nothing) loc
  return $ TH.VarE 'toByte6809 `TH.AppE` loc'

-- ToArray specialized to this cpu

type ToArray6809 e = ToArray PExprCpu6809 e

toArray6809 :: ToArray6809 a => SourcePos -> a -> Expr6809
toArray6809 = toArray

array6809 :: TH.Q TH.Exp
array6809 = do
  loc <- getPosition
  loc' <- dataToExpQ (const Nothing) loc
  return $ TH.VarE 'toArray6809 `TH.AppE` loc'

-- ToStructOrUnion specialized to this cpu

type ToStructOrUnion6809 e = ToStructOrUnion PExprCpu6809 e

toStructOrUnion6809 :: ToStructOrUnion6809 a => SourcePos -> a -> Expr6809
toStructOrUnion6809 = toStructOrUnion

structOrUnion6809 :: TH.Q TH.Exp
structOrUnion6809 = do
  loc <- getPosition
  loc' <- dataToExpQ (const Nothing) loc
  return $ TH.VarE 'toStructOrUnion6809 `TH.AppE` loc'

-- specialized versions of structOrUnion6809

struct6809 :: TH.Q TH.Exp
struct6809 = do
  loc <- getPosition
  loc' <- dataToExpQ (const Nothing) loc
  t <- [t|[(Text, Expr6809)] -> Expr6809|]
  return $
    TH.SigE
      (TH.VarE 'toStructOrUnion6809 `TH.AppE` loc')
      t

toUnion6809 :: ToExpr PExprCpu6809 a => SourcePos -> Text -> a -> Expr6809
toUnion6809 loc k v = (loc, PEUserStructOrUnion (M.singleton k (toExpr loc v)))

union6809 :: TH.Q TH.Exp
union6809 = do
  loc <- getPosition
  loc' <- dataToExpQ (const Nothing) loc
  return $ TH.VarE 'toUnion6809 `TH.AppE` loc'

-- the compile function

compile6809 :: Asm6809 -> CompilerResult Cpu6809
compile6809 = compileGeneric'
