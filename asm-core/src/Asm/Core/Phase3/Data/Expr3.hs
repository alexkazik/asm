{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Asm.Core.Phase3.Data.Expr3
  ( Expr3(..)
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Data.ByteVal
import           Asm.Core.Data.CpuData
import           Asm.Core.Data.FunctionKey
import           Asm.Core.Data.Reference
import           Asm.Core.Data.Ternary
import           Asm.Core.Data.TypeDefinition
import           Asm.Core.PrettyPrint
import           Asm.Core.SourcePos
import           Asm.Data.ByteValSimple

data Expr3 c
  -- constant data
  = E3ConstInt Location !Int64
  | E3ConstMaskedInt Location !TInt64
  | E3ConstBool Location !Bool
  | E3ByteVal Location !(ByteVal (Expr3 c))
  -- CPU specific expresion
  | E3CpuExpr Location !(CE3 c)
  -- reference to a label/variable/const/pointer/pool/...
  | E3Pointer Location !Reference !TypeDefinition !Int64
  -- structure and dereferencing
  | E3UserArray Location !(Vector (Expr3 c)) !(Maybe (Expr3 c))
  | E3UserArrayBVS Location !(SVector ByteValSimple) !(Maybe (Expr3 c))
  | E3DerefArray Location !(Expr3 c) !(Expr3 c)
  | E3DefineArray Location !(Expr3 c) !(Maybe (Expr3 c))

  | E3TypeStruct Location ![(Maybe Text, Expr3 c)]
  | E3TypeUnion Location ![(Maybe Text, Expr3 c)]

  -- Expressions while compiling
  -- structure and dereferencing
  | E3UserStructOrUnion Location !(Map Text (Expr3 c)) !(Maybe (Expr3 c))
  | E3DerefStruct Location !(Expr3 c) !Text
  -- functions
  | E3Function Location !FunctionKey ![Expr3 c]
  -- Reference to Namespace/Variable/TypeDefinition
  | E3LabelRef Location !Reference
  | E3MagicValue Location !Text

instance CpuData c => Show (Expr3 c) where
  show = showPrettySrc

instance CpuData c => PrettySrc (Expr3 c) where
  prettySrcM expr = do
    addFirstSourceLine (locationOf expr)
    go expr
    where
      go (E3ConstInt _ i) = return $ pretty i
      go (E3ConstMaskedInt _ i) = return $ pshow i
      go (E3ConstBool _ i) = return $ pretty i
      go E3ByteVal{} = return "!EByteVal"
      go E3CpuExpr{} = return "!ECpuExpr"
      go (E3Pointer _ l _ _)      = return $ pretty l
      go E3UserArray{} = return "!EUserArray"
      go E3UserArrayBVS{} = return "!EUserArrayBVS"
      go (E3DerefArray _ s e) = do
        s' <- prettySrcM s
        e' <- prettySrcM e
        return $ s' ++ "[" ++ e' ++ "]"
      go (E3DefineArray _ s e) = do
        s' <- prettySrcM s
        e' <- mapM prettySrcM e
        return $ s' ++ "[" ++ fromMaybe "" e' ++ "]"
      go E3TypeStruct{} = return "!ETypeStruct"
      go E3TypeUnion{} = return "!ETypeUnion"
      go E3UserStructOrUnion{} = return "!EUserStructOrUnion"
      go (E3DerefStruct _ s e) = do
        s' <- prettySrcM s
        return $ s' ++ "." ++ pretty e
      go (E3Function _ f ps)      = do
        ps' <- mapM prettySrcM ps
        let isOp = all (`elem` ("+-~!*/%<>=&^|" :: String)) (functionKeyName f)
        case (isOp, ps') of
          (True, [l, r]) -> return $ "(" ++ l ++ " " ++ pshow f ++ " " ++ r ++ ")"
          (True, [r])    -> return $ "(" ++ pshow f ++ " " ++ r ++ ")"
          _              -> return $ pshow f ++ pretty '(' ++ encloseSep mempty mempty ", " ps' ++ pretty ')'
      go (E3LabelRef _ l)      = return $ pretty l
      go (E3MagicValue _ v) = return $ pretty '#' ++ pretty v

instance LocationOf (Expr3 c) where
  locationOf (E3ConstInt loc _)            = loc
  locationOf (E3ConstMaskedInt loc _)      = loc
  locationOf (E3ConstBool loc _)           = loc
  locationOf (E3ByteVal loc _)             = loc
  locationOf (E3CpuExpr loc _)             = loc
  locationOf (E3Pointer loc _ _ _)         = loc
  locationOf (E3UserArray loc _ _)         = loc
  locationOf (E3UserArrayBVS loc _ _)      = loc
  locationOf (E3DerefArray loc _ _)        = loc
  locationOf (E3DefineArray loc _ _)       = loc
  locationOf (E3TypeStruct loc _)          = loc
  locationOf (E3TypeUnion loc _)           = loc
  locationOf (E3UserStructOrUnion loc _ _) = loc
  locationOf (E3DerefStruct loc _ _)       = loc
  locationOf (E3Function loc _ _)          = loc
  locationOf (E3LabelRef loc _)            = loc
  locationOf (E3MagicValue loc _)          = loc
