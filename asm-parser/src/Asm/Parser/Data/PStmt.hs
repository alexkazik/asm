module Asm.Parser.Data.PStmt
  ( PStmt
  , PStmtI(..)
  , isPSNothing
  ) where

import           Asm.Core.Prelude
import           Language.Haskell.TH          (Exp)

import           Asm.Core.Data.VariableType
import           Asm.Core.SourcePos

import           Asm.Parser.Data.Int64Value
import           Asm.Parser.Data.LabelIdValue
import           Asm.Parser.Data.PExpr

data PStmtI ps pe
  = PSBuildIf !(PExpr pe)
  | PSBuildElseif !(PExpr pe)
  | PSBuildElse
  | PSBuildEndif
  | PSBuildNamespace !(Maybe LabelIdValue)
  | PSBuildBlock !(Maybe LabelIdValue) !(Maybe (PExpr pe))
  | PSBuildEnd
  | PSBuildDirectIf !Exp
  | PSBuildDirectElseif !Exp
  | PSBuildDirectElse
  | PSBuildDirectEndif
  | PSBuildFor !LabelIdValue !(PExpr pe) !Bool !(PExpr pe) !(PExpr pe)
  | PSAntiBuildDirectIfBlock ![(Exp, [PStmt ps pe])]
  | PSAntiNamespace !Exp !(Maybe LabelIdValue)
  | PSAntiBlock !Exp !(Maybe LabelIdValue) !(Maybe (PExpr pe))
  | PSIfBlock ![(PExpr pe, [PStmt ps pe])]
  | PSNamespace !(Maybe LabelIdValue) ![PStmt ps pe]
  | PSBlock !(Maybe LabelIdValue) !(Maybe (PExpr pe)) ![PStmt ps pe]
  | PSFor !LabelIdValue !(PExpr pe) !Bool !(PExpr pe) !(PExpr pe) ![PStmt ps pe]
  | PSLabelDefinition !LabelIdValue
  | PSCpuStmt !ps
  | PSMetaSet !(PExpr pe) !(PExpr pe)
  | PSMetaUnset !(PExpr pe)
  | PSMetaSticky !(PExpr pe)
  | PSMetaUnsticky !(PExpr pe)
  | PSVariable !VariableType !LabelIdValue !(PExpr pe) !(Maybe (PExpr pe)) !(Maybe (PExpr pe)) !Int64Value !(Maybe Int64Value)
  | PSPoolStmt !LabelIdValue ![LabelIdValue] !Bool !Int64Value !Int64Value
  | PSTypeDef !LabelIdValue !(PExpr pe)
  | PSAlias !LabelIdValue !(PExpr pe)
  | PSTraceStep !(PStmt ps pe)
  | PSNothing
  deriving (Show,Typeable,Data)

type PStmt ps pe = (SourcePos, PStmtI ps pe)

isPSNothing :: PStmt ps pe -> Bool
{-# INLINABLE isPSNothing #-}
isPSNothing (_, PSNothing) = True
isPSNothing _              = False
