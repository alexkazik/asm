{-# LANGUAGE NoImplicitPrelude #-}

module Asm.Core.Phase2.Data.Stmt2
  ( Stmt2(..)
  , ForCmp(..)
  , Stmt2Block
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Data.CpuData
import           Asm.Core.Data.Reference
import           Asm.Core.Data.VariableType
import           Asm.Core.Phase1.Data.Expr12
import           Asm.Core.Phase1.Data.Stmt1  (ForCmp (..))
import           Asm.Core.SourcePos

data Stmt2 c
  = S2IfBlock !Location ![(Text, Expr12 c, Stmt2Block c)]
  | S2Namespace !Location !Text !(Stmt2Block c)
  | S2Block !Location !Text !(Maybe (Expr12 c)) !(Stmt2Block c)
  | S2For !Location !Text !Reference !(Expr12 c) !ForCmp !(Expr12 c) !(Expr12 c) !(Stmt2Block c)
  | S2LabelDefinition !Location !Reference
  | S2CpuStmt !Location !(CS12 c)
  | S2MetaSet !Location !(Expr12 c) !(Expr12 c)
  | S2MetaUnset !Location !(Expr12 c)
  | S2MetaSticky !Location !(Expr12 c)
  | S2MetaUnsticky !Location !(Expr12 c)
  | S2Variable !Location !VariableType !Reference !(Expr12 c) !(Maybe (Expr12 c)) !(Maybe (Expr12 c)) !Int64 !(Maybe Int64)
  | S2TypeDef !Location !Reference !(Expr12 c)

type Stmt2Block c = [Stmt2 c]
