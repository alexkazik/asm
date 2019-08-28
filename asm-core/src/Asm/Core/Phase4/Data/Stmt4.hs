{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Asm.Core.Phase4.Data.Stmt4
    ( Stmt4(..)
    , ForCmp(..)
    , Stmt4Block
    , dumpStmtBlock
    , locationOf
    ) where

import           Asm.Core.Prelude

import           Asm.Core.Data.CpuData
import           Asm.Core.Data.Reference
import           Asm.Core.Phase1.Data.Stmt1 (ForCmp (..))
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.PrettyPrint
import           Asm.Core.SourcePos

data Stmt4 c
  = S4IfBlock !Location ![(Text, Expr4 c, Stmt4Block c)]
  | S4LabelDefinition !Location !Reference
  | S4CpuStmt !Location !(CS4 c)
  | S4For !Location !Reference !(Expr4 c) !ForCmp !(Expr4 c) !(Expr4 c) !(Stmt4Block c)

deriving instance CpuData c => Eq (Stmt4 c)

type Stmt4Block c = [Stmt4 c]

instance CpuData c => PrettySrc (Stmt4 c) where
  prettySrcM (S4IfBlock _loc (blk:blks)) = do
    blk' <- prettyIfBlock False blk
    blks' <- mapM (prettyIfBlock True) blks
    return $ vsep (blk':blks' ++ ["end"])
  prettySrcM (S4IfBlock _loc []) = return mempty
  prettySrcM (S4LabelDefinition loc l) = do
    addFirstSourceLine loc
    s <- getSourceLines
    return $ vsep [s, pretty l ++ pretty ':']
  prettySrcM (S4CpuStmt loc s) = do
    addFirstSourceLine loc
    s' <- prettySrcM s
    sp <- getSourceLines
    return $ vsep [sp, "  " ++ s']
  prettySrcM (S4For loc _n _f _c _t _s blk) = do
    addFirstSourceLine loc
    s <- getSourceLines
    blk' <- mapM prettySrcM blk
    return $ vsep
      [ s
      , "  for"
      , indent 4 (vsep blk')
      , "  end"
      ]

prettyIfBlock :: CpuData c => Bool -> (Text, Expr4 c, Stmt4Block c) -> PPSM Doc
prettyIfBlock ei (_t, e, sb) = do
  e' <- prettySrcM e
  s <- getSourceLines
  sb' <- mapM prettySrcM sb
  return $ vsep [s, bool "  if " "  elseif " ei ++ e', indent 4 (vsep sb')]

dumpStmtBlock :: PrettySrc a => [a] -> Doc
dumpStmtBlock a = vsep $ evalState (mapM prettySrcM a) (PrettyPrintState Nothing [])

instance LocationOf (Stmt4 c) where
  locationOf (S4IfBlock loc _)         = loc
  locationOf (S4LabelDefinition loc _) = loc
  locationOf (S4CpuStmt loc _)         = loc
  locationOf (S4For loc _ _ _ _ _ _)   = loc
