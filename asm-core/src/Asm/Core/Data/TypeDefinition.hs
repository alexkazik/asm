{-# LANGUAGE NoImplicitPrelude #-}

module Asm.Core.Data.TypeDefinition
  ( TypeDefinition(..)
  , showTdAsParam
  , poolType
  ) where

import           Asm.Core.Prelude

import           Asm.Core.SourcePos

data TypeDefinition
  = TDTypeRef !Location ![Text]
  | TDStruct ![(Maybe Text, TypeDefinition)]
  | TDUnion ![(Maybe Text, TypeDefinition)]
  | TDArray !TypeDefinition !(Maybe Int64)
  | TDByte
  | TDCode
  | TDPool Bool Bool Bool
  | TDBlock
  -- virtual types for expression executing
  | TDBool
  | TDInt
  | TDMaskedInt
  -- type not yet determined
  | TDInvalid
  deriving (Eq, Ord)

instance Show TypeDefinition where
  show (TDTypeRef loc name) = "#" ++ show name ++ sourcePosShortListS loc
  show (TDStruct str) = "#struct { " ++ intercalate "; " (map (\(t,d) -> show d ++ " " ++ maybe "*unnamed*" unpack t) str) ++ " }"
  show (TDUnion str) = "#union { " ++ intercalate "; " (map (\(t,d) -> show d ++ " " ++ maybe "*unnamed*" unpack t) str) ++ " }"
  show (TDArray t (Just l)) = "#" ++ show t ++ "[" ++ show l ++ "]"
  show (TDArray t Nothing) = "#" ++ show t ++ "[]"
  show TDByte = "#byte"
  show (TDPool False False False) = "#Pool:INVALID"
  show (TDPool False False True) = "#Pool:Type"
  show (TDPool False True False) = "#Pool:Element"
  show (TDPool False True True) = "#Pool:Element:virtual"
  show (TDPool True False False) = "#Pool:Container"
  show (TDPool True False True) = "#Pool:Container:virtual"
  show (TDPool True True False) = "#Pool:Container+Element"
  show (TDPool True True True) = "#Pool:Container+Element:virtual"
  show TDBlock = "#Block"

  show TDCode = "#code"
  show TDBool = "#bool"
  show TDInt = "#int"
  show TDMaskedInt = "#maskedint"
  show TDInvalid = "#INVALID"

showTdAsParam :: TypeDefinition -> String
showTdAsParam TDByte                    = "Byte"
showTdAsParam TDCode                    = "Code"
showTdAsParam TDBool                    = "Bool"
showTdAsParam TDInt                     = "Int"
showTdAsParam TDMaskedInt               = "MaskedInt"
showTdAsParam (TDPool False False True) = "Pool"
showTdAsParam _                         = "Invalid (TD)"

poolType :: TypeDefinition
{-# INLINE poolType #-}
poolType = TDPool False False True
