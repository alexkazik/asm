{-# LANGUAGE FlexibleInstances #-}

module Asm.Parser.Data.PExpr
  ( PExpr
  , PExprI(..)
  , dumpExpr
  ) where

import           Asm.Core.Prelude
import           Language.Haskell.TH          (Exp)

import           Asm.Core.Data.ByteVal
import           Asm.Core.Data.Ternary
import           Asm.Core.SourcePos
import           Asm.Data.ByteValSimple

import           Asm.Parser.Data.LabelIdValue

data PExprI pe
  -- constant data
  = PEConstInt !Int64
  | PEConstMaskedInt !TInt64
  | PEConstBool !Bool
  | PEByteVal !(ByteVal (PExpr pe))
  --  -- CPU specific expresion
  | PECpuExpr !pe
  --  -- structure and dereferencing
  | PEUserArrayL ![PExpr pe]
  | PEUserArray !(Vector (PExpr pe))
  | PEUserArrayBVS !(SVector ByteValSimple)
  | PEDerefArray !(PExpr pe) !(PExpr pe)
  | PEDefineArray !(PExpr pe) !(Maybe (PExpr pe))
  --
  | PETypeStruct ![(Maybe LabelIdValue, PExpr pe)]
  | PETypeUnion ![(Maybe LabelIdValue, PExpr pe)]

  --  -- Expressions while parsing
  --  -- structure and dereferencing
  | PEUserStructOrUnion !(Map Text (PExpr pe))
  | PEDerefStruct !(PExpr pe) !LabelIdValue
  --  -- functions
  | PEUOperator !Text !(PExpr pe)
  | PEBOperator !Text !(PExpr pe) !(PExpr pe)
  | PEFunction !LabelIdValue ![PExpr pe]
  --  -- Reference to Namespace/Variable/TypeDefinition
  | PELabelId !LabelIdValue
  --  -- created in parser, removed during quotation
  | PEAntiExpr !Exp
  | PEAntiArray !Exp
  | PEAntiStruct !Exp
  --  -- only used until convert***
  | PETraceStep !(PExpr pe)
  | PEMagicValue !Text
  deriving (Show,Typeable,Data,Eq)

type PExpr pe = (SourcePos, PExprI pe)

instance Num (PExpr pe) where
  fromInteger x = (initialSourcePos "number literal", PEConstInt $ fromInteger x)
  _ + _ = errorWithoutStackTrace "PExpr.+: unsupported"
  _ - _ = errorWithoutStackTrace "PExpr.-: unsupported"
  _ * _ = errorWithoutStackTrace "PExpr.*: unsupported"
  negate _ = errorWithoutStackTrace "PExpr.negate: unsupported"
  abs _ = errorWithoutStackTrace "PExpr.abs: unsupported"
  signum _ = errorWithoutStackTrace "PExpr.signum: unsupported"


dumpExpr :: Show pe => PExpr pe -> String
dumpExpr (_, PEDerefStruct e s)   = dumpExpr e ++ "->" ++ show s
dumpExpr (_, PEDerefArray e a)    = dumpExpr e ++ "[" ++ dumpExpr a ++ "]"
dumpExpr (_, PEConstInt i)        = show i
dumpExpr (_, PEConstMaskedInt mi) = show mi
dumpExpr (_, PEUOperator o e)     = unpack o ++ dumpExpr e
dumpExpr (_, PEBOperator o e1 e2) = "(" ++ dumpExpr e1 ++ " " ++ unpack o ++ " " ++ dumpExpr e2 ++ ")"
dumpExpr (_, PEFunction n es)     = show n ++ "(" ++ intercalate ", " (map dumpExpr es) ++ ")"
dumpExpr (_, PEConstBool b)       = if b then "true" else "false"
dumpExpr (_, e)                   = show e
