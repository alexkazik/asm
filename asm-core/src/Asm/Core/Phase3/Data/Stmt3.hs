module Asm.Core.Phase3.Data.Stmt3
  ( Stmt3(..)
  , ForCmp(..)
  , Stmt3Block
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Data.CpuData
import           Asm.Core.Data.Reference
import           Asm.Core.Data.TypeDefinition
import           Asm.Core.Data.VariableType
import           Asm.Core.Phase1.Data.Stmt1   (ForCmp (..))
import           Asm.Core.Phase3.Data.Expr3
import           Asm.Core.SourcePos

data Stmt3 c
  = S3IfBlock !Location ![(Text, Expr3 c, Stmt3Block c)]
  | S3Namespace !Location !Text !(Stmt3Block c)
  | S3Block !Location !Text !(Maybe (Expr3 c)) !(Stmt3Block c)
  | S3For !Location !Text !Reference !(Expr3 c) !ForCmp !(Expr3 c) !(Expr3 c) !(Stmt3Block c)
  | S3LabelDefinition !Location !Reference
  | S3CpuStmt !Location !(CS3 c)
  | S3MetaSet !Location !(Expr3 c) !(Expr3 c)
  | S3MetaUnset !Location !(Expr3 c)
  | S3MetaSticky !Location !(Expr3 c)
  | S3MetaUnsticky !Location !(Expr3 c)
  | S3Variable !Location !VariableType !Reference !TypeDefinition !(Maybe (Expr3 c)) !(Maybe (Expr3 c)) !Int64 !(Maybe Int64)
  | S3VariableUnresolved !Location !VariableType !Reference !(Expr3 c) !(Maybe (Expr3 c)) !(Maybe (Expr3 c)) !Int64 !(Maybe Int64)

type Stmt3Block c = [Stmt3 c]
