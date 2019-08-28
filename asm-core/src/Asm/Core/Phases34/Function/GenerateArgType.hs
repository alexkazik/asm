{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Asm.Core.Phases34.Function.GenerateArgType
  ( ArgType(..)
  , typeToAT
  , returnAT
  , atToTDName
  , atToExpConstName
  , spliceATList
  ) where

import           Asm.Core.Prelude
import           Language.Haskell.TH

import           Asm.Core.Control.CompilerError
import           Asm.Core.Data.ByteVal
import           Asm.Core.Data.Ternary
import           Asm.Core.Data.TypeDefinition
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.PrettyPrint
import           Asm.Core.SourcePos
import           Asm.Data.InfInt64

-- | 'ArgType': a type to represent all types supported by this generator
data ArgType
  = ATInt64
  | ATInt64AsInt -- ^ auto conversion Int64(assembler) to Int(function)
  | ATMaskedInt -- ^ not for detecting function arguments, only for passing arguments and return type
  | ATMaskedIntOrInt -- ^ only temporary, will be split in 'ATMaskedInt' and 'ATInt64' by 'spliceATList' (or just 'ATMaskedInt' as a return type)
  | ATIntAsMaskedInt -- ^ for auto conversion, see 'ATMaskedIntOrInt'
  | ATBool
  | ATByte
  | ATLoc
  | ATRangedInt
  | ATRangedIntOrInt
  | ATIntAsRangedInt
  deriving (Eq, Show)

instance Pretty ArgType where
  pretty = pshow

-- | A function to convert Type Names to the 'ArgType'
typeToAT :: Name -> ArgType
typeToAT t
  -- assembler native types
  | t == ''Int64 = ATInt64
  | t == ''TInt64 = ATMaskedIntOrInt
  | t == ''Bool = ATBool
  | t == ''ByteVal = ATByte
  -- will be automatic converted
  | t == ''Int = ATInt64AsInt
  -- for location passing only
  | t == ''Location = ATLoc
  -- ranged int
  | t == ''InfInt64 = ATRangedIntOrInt
  -- (currently) unsupported types
  | otherwise = [printInternalError|Unknown type: $t|]

-- | Retrieve the 'ArgType' for the function result
returnAT :: ArgType -> ArgType
returnAT ATInt64          = ATInt64
returnAT ATMaskedIntOrInt = ATMaskedInt
returnAT ATBool           = ATBool
returnAT ATByte           = ATByte
returnAT ATRangedIntOrInt = ATRangedIntOrInt
returnAT a                = [printInternalError|Unsupported type: $a|]

-- | Retrieve the 'TypeDefinition' constructor 'Name' for a given 'ArgType'
atToTDName :: String -> ArgType -> Name
atToTDName _ ATInt64          = 'TDInt
atToTDName _ ATInt64AsInt     = 'TDInt
atToTDName _ ATMaskedInt      = 'TDMaskedInt
atToTDName _ ATIntAsMaskedInt = 'TDInt
atToTDName _ ATBool           = 'TDBool
atToTDName _ ATByte           = 'TDByte
atToTDName _ ATRangedInt      = 'TDInt
atToTDName _ ATIntAsRangedInt = 'TDInt
atToTDName x a                = [printInternalError|atToTDName: invalid argument: $a in $x|]

-- | Retrieve the 'Expr' constructor 'Name' which binds the value of a given 'ArgType'
atToExpConstName :: String -> ArgType -> Name
atToExpConstName _ ATMaskedInt      = 'E4ConstMaskedInt
atToExpConstName _ ATIntAsMaskedInt = 'E4ConstInt
atToExpConstName _ ATInt64          = 'E4ConstInt
atToExpConstName _ ATBool           = 'E4ConstBool
atToExpConstName _ ATByte           = 'E4ByteVal
atToExpConstName _ ATInt64AsInt     = 'E4ConstInt
atToExpConstName _ ATIntAsRangedInt = 'E4ConstInt
atToExpConstName x a                = [printInternalError|atToExpConstName: invalid argument: $a in $x|]

-- | Replace each 'ATMaskedIntOrInt' in the parameter list with 'ATMaskedInt' in one and 'ATIntAsMaskedInt' in another,
-- | also replace each 'ATRangedIntOrInt' in the parameter list with 'ATRangedInt' in one and 'ATIntAsRangedInt' in another,
-- | thus returning a list of parameter lists
spliceATList :: [ArgType] -> [[ArgType]]
spliceATList = spliceATList' []
  where
    spliceATList' :: [ArgType] -> [ArgType] -> [[ArgType]]
    spliceATList' pfx [] = [pfx]
    spliceATList' pfx (ATMaskedIntOrInt:sfx) = spliceATList' (pfx++[ATMaskedInt]) sfx ++ spliceATList' (pfx++[ATIntAsMaskedInt]) sfx
    spliceATList' pfx (ATRangedIntOrInt:sfx) = spliceATList' (pfx++[ATRangedInt]) sfx ++ spliceATList' (pfx++[ATIntAsRangedInt]) sfx
    spliceATList' pfx (s:sfx) = spliceATList' (pfx++[s]) sfx
