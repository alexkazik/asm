module Asm.Core.Phase1.Data.Stmt1
  ( Stmt1(..)
  , ForCmp(..)
  , Stmt1Block
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Data.CpuData
import           Asm.Core.Data.VariableType
import           Asm.Core.Phase1.Data.Expr12
import           Asm.Core.SourcePos

data ForCmp
  = ForCmpLessThan
  | ForCmpLessEqual
  deriving (Data, Eq, Typeable)

data Stmt1 c
  = S1IfBlock !Location ![(Text, Expr12 c, Stmt1Block c)]
  | S1Namespace !Location !(Maybe Text) !(Stmt1Block c)
  | S1Block !Location !(Maybe Text) !(Maybe (Expr12 c)) !(Stmt1Block c)
  | S1For !Location !Text !(Expr12 c) !ForCmp !(Expr12 c) !(Expr12 c) !(Stmt1Block c)
  | S1LabelDefinition !Location !Text
  | S1CpuStmt !Location !(CS12 c)
  | S1MetaSet !Location !(Expr12 c) !(Expr12 c)
  | S1MetaUnset !Location !(Expr12 c)
  | S1MetaSticky !Location !(Expr12 c)
  | S1MetaUnsticky !Location !(Expr12 c)
  | S1Variable !Location !VariableType !Text !(Expr12 c) !(Maybe (Expr12 c)) !(Maybe (Expr12 c)) !Int64 !(Maybe Int64)
  | S1PoolStmt !Location !Text ![Text] !Bool !Int64 !Int64
  | S1TypeDef !Location !Text !(Expr12 c)
  | S1Alias !Location !Text !(Expr12 c)

type Stmt1Block c = [Stmt1 c]

instance LocationOf (Stmt1 c) where
  locationOf (S1IfBlock loc _)              = loc
  locationOf (S1Namespace loc _ _)          = loc
  locationOf (S1Block loc _ _ _)            = loc
  locationOf (S1For loc _ _ _ _ _ _)        = loc
  locationOf (S1LabelDefinition loc _)      = loc
  locationOf (S1CpuStmt loc _)              = loc
  locationOf (S1MetaSet loc _ _)            = loc
  locationOf (S1MetaUnset loc _)            = loc
  locationOf (S1MetaSticky loc _)           = loc
  locationOf (S1MetaUnsticky loc _)         = loc
  locationOf (S1Variable loc _ _ _ _ _ _ _) = loc
  locationOf (S1TypeDef loc _ _)            = loc
  locationOf (S1PoolStmt loc _ _ _ _ _)     = loc
  locationOf (S1Alias loc _ _)              = loc
