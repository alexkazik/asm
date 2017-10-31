module Asm.Core.Phase1.Data.Expr12
  ( Expr12(..)
  , updateLocation
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Data.ByteVal
import           Asm.Core.Data.CpuData
import           Asm.Core.Data.Ternary
import           Asm.Core.Data.TypeDefinition
import           Asm.Core.PrettyPrint
import           Asm.Core.SourcePos
import           Asm.Data.ByteValSimple

data Expr12 c
  -- constant data
  = E12ConstInt Location !Int64
  | E12ConstMaskedInt Location !TInt64
  | E12ConstBool Location !Bool
  | E12ByteVal Location !(ByteVal (Expr12 c))
  -- CPU specific expresion
  | E12CpuExpr Location !(CE12 c)
  -- reference to a label/variable/const/pointer/pool/...
  | E12Pointer Location !Text !TypeDefinition !Int64
  -- structure and dereferencing
  | E12UserArray Location !(Vector (Expr12 c)) !(Maybe (Expr12 c))
  | E12UserArrayBVS Location !(SVector ByteValSimple) !(Maybe (Expr12 c))
  | E12DerefArray Location !(Expr12 c) !(Expr12 c)
  | E12DefineArray Location !(Expr12 c) !(Maybe (Expr12 c))

  | E12TypeStruct Location ![(Maybe Text, Expr12 c)]
  | E12TypeUnion Location ![(Maybe Text, Expr12 c)]

  -- Expressions while compiling
  -- structure and dereferencing
  | E12UserStructOrUnion Location !(Map Text (Expr12 c)) !(Maybe (Expr12 c))
  | E12DerefStruct Location !(Expr12 c) !Text
  -- functions
  | E12Function Location !Text ![Expr12 c]
  -- Reference to Namespace/Variable/TypeDefinition
  | E12LabelRef Location !Text
  | E12MagicValue Location !Text

instance CpuData c => Show (Expr12 c) where
  show = showPrettySrc

instance CpuData c => PrettySrc (Expr12 c) where
  prettySrcM expr = do
    addFirstSourceLine (locationOf expr)
    go expr
    where
      go (E12ConstInt _ i) = return $ pretty i
      go (E12ConstMaskedInt _ i) = return $ pshow i
      go (E12ConstBool _ i) = return $ pretty i
      go E12ByteVal{} = return "!EByteVal"
      go E12CpuExpr{} = return "!ECpuExpr"
      go (E12Pointer _ l _ _)      = return $ pretty l
      go E12UserArray{} = return "!EUserArray"
      go E12UserArrayBVS{} = return "!EUserArrayBVS"
      go (E12DerefArray _ s e) = do
        s' <- prettySrcM s
        e' <- prettySrcM e
        return $ s' ++ "[" ++ e' ++ "]"
      go (E12DefineArray _ s e) = do
        s' <- prettySrcM s
        e' <- mapM prettySrcM e
        return $ s' ++ "[" ++ fromMaybe "" e' ++ "]"
      go E12TypeStruct{} = return "!ETypeStruct"
      go E12TypeUnion{} = return "!ETypeUnion"
      go E12UserStructOrUnion{} = return "!EUserStructOrUnion"
      go (E12DerefStruct _ s e) = do
        s' <- prettySrcM s
        return $ s' ++ "." ++ pretty e
      go (E12Function _ f ps)      = do
        ps' <- mapM prettySrcM ps
        let isOp = all (`elem` ("+-~!*/%++=&^|" :: String)) f
        case (isOp, ps') of
          (True, [l, r]) -> return $ "(" ++ l ++ " " ++ pretty f ++ " " ++ r ++ ")"
          (True, [r])    -> return $ "(" ++ pretty f ++ " " ++ r ++ ")"
          _              -> return $ pretty f ++ pretty '(' ++ encloseSep mempty mempty ", " ps' ++ pretty ')'
      go (E12LabelRef _ l)      = return $ pretty l
      go (E12MagicValue _ v) = return $ pretty '#' ++ pretty v

instance LocationOf (Expr12 c) where
  locationOf (E12ConstInt loc _)            = loc
  locationOf (E12ConstMaskedInt loc _)      = loc
  locationOf (E12ConstBool loc _)           = loc
  locationOf (E12ByteVal loc _)             = loc
  locationOf (E12CpuExpr loc _)             = loc
  locationOf (E12Pointer loc _ _ _)         = loc
  locationOf (E12UserArray loc _ _)         = loc
  locationOf (E12UserArrayBVS loc _ _)      = loc
  locationOf (E12DerefArray loc _ _)        = loc
  locationOf (E12DefineArray loc _ _)       = loc
  locationOf (E12TypeStruct loc _)          = loc
  locationOf (E12TypeUnion loc _)           = loc
  locationOf (E12UserStructOrUnion loc _ _) = loc
  locationOf (E12DerefStruct loc _ _)       = loc
  locationOf (E12Function loc _ _)          = loc
  locationOf (E12LabelRef loc _)            = loc
  locationOf (E12MagicValue loc _)          = loc

updateLocation :: Location -> Expr12 c -> Expr12 c
updateLocation loc (E12ConstInt _ a)            = E12ConstInt loc a
updateLocation loc (E12ConstMaskedInt _ a)      = E12ConstMaskedInt loc a
updateLocation loc (E12ConstBool _ a)           = E12ConstBool loc a
updateLocation loc (E12ByteVal _ a)             = E12ByteVal loc a
updateLocation loc (E12CpuExpr _ a)             = E12CpuExpr loc a
updateLocation loc (E12Pointer _ a b c)         = E12Pointer loc a b c
updateLocation loc (E12UserArray _ a b)         = E12UserArray loc a b
updateLocation loc (E12UserArrayBVS _ a b)      = E12UserArrayBVS loc a b
updateLocation loc (E12DerefArray _ a b)        = E12DerefArray loc a b
updateLocation loc (E12DefineArray _ a b)       = E12DefineArray loc a b
updateLocation loc (E12TypeStruct _ a)          = E12TypeStruct loc a
updateLocation loc (E12TypeUnion _ a)           = E12TypeUnion loc a
updateLocation loc (E12UserStructOrUnion _ a b) = E12UserStructOrUnion loc a b
updateLocation loc (E12DerefStruct _ a b)       = E12DerefStruct loc a b
updateLocation loc (E12Function _ a b)          = E12Function loc a b
updateLocation loc (E12LabelRef _ a)            = E12LabelRef loc a
updateLocation loc (E12MagicValue _ a)          = E12MagicValue loc a
