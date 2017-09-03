{-# LANGUAGE FlexibleInstances #-}

module Asm.Core.Data.KindDefinition
  ( KindDefinition(..)
  , showKdAsParam
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Data.MetaKey
import           Asm.Core.Data.TypeDefinition
import           Asm.Core.PrettyPrint
import           Asm.Core.SourcePos

data KindDefinition
  = KDNamespace
  | KDPointer !TypeDefinition
  | KDData !TypeDefinition
  | KDType !TypeDefinition
  | KDTypeInExpr
  | KDUserArray
  | KDUserStructOrUnion
  | KDAlias
  | KDCpu
  | KDMeta !MetaKey
  | KDLoopVariable
  | KDMagicValue
  deriving (Eq,Show)

instance Pretty KindDefinition where
  pretty KDNamespace         = "*Namespace"
  pretty (KDPointer typ)     = "*Pointer " ++ pshow typ
  pretty (KDData typ)        = "*Data " ++ pshow typ
  pretty (KDType t)          = "*Type " ++ pshow t
  pretty KDTypeInExpr        = "*Type@Expr"
  pretty KDUserArray         = "*UserArray"
  pretty KDUserStructOrUnion = "*UserStructOrUnion"
  pretty KDAlias             = "*Alias"
  pretty KDCpu               = "*Cpu"
  pretty (KDMeta t)          = "*Meta " ++ pshow t
  pretty KDLoopVariable      = "*LoopVariable"
  pretty KDMagicValue        = "*MagicValue"

instance (PrettySrc p) => PrettySrc (KindDefinition, p) where
  prettySrcM (k, e) = do
    let k' = pretty k
    e' <- prettySrcM e
    return $"(" ++ k' ++ ", " ++ e' ++ ")"

instance Pretty (Location, KindDefinition) where
  pretty (l, k) = pretty k <+> pretty (sourcePosShortList l)

showKdAsParam :: KindDefinition -> String
showKdAsParam (KDPointer td)      = "Pointer of type " ++ showTdAsParam td
showKdAsParam (KDData td)         = showTdAsParam td
showKdAsParam KDType{}            = "Type"
showKdAsParam KDTypeInExpr        = "Type"
showKdAsParam KDUserArray         = "Array"
showKdAsParam KDUserStructOrUnion = "StructOrUnion"
showKdAsParam KDMeta{}            = "MetaKey"
showKdAsParam KDMagicValue        = "MagicValue"
showKdAsParam _                   = "Invalid (KD)"
