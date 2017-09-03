module Asm.Core.Phase4.Data.Expr4
  ( Expr4(..)
  ) where

import           Asm.Core.Prelude
import qualified Data.Vector.Storable         as SV

import           Asm.Core.Data.ByteVal
import           Asm.Core.Data.CpuData
import           Asm.Core.Data.FunctionKey
import           Asm.Core.Data.MetaKey
import           Asm.Core.Data.Reference
import           Asm.Core.Data.Ternary
import           Asm.Core.Data.TypeDefinition
import           Asm.Core.PrettyPrint
import           Asm.Core.SourcePos
import           Asm.Data.ByteValSimple
import           Asm.Data.InfInt64

data Expr4 c
  -- constant data
  = E4ConstInt Location !Int64
  | E4RangedInt Location !InfInt64 !InfInt64 !(Maybe (Reference, Int64)) !(Expr4 c)
  | E4ConstMaskedInt Location !TInt64
  | E4ConstBool Location !Bool
  | E4ByteVal Location !(ByteVal (Expr4 c))
  -- CPU specific expresion
  | E4CpuExpr Location !(CE4 c)
  -- reference to a type
  | E4Type Location !TypeDefinition !Int64
  -- reference to a label/variable/const/pointer/pool/...
  | E4Pointer Location !Reference !TypeDefinition !Int64
  -- structure and dereferencing
  | E4UserArray Location !(Vector (Expr4 c)) !(Maybe (Expr4 c))
  | E4UserArrayBVS Location !(SV.Vector ByteValSimple) !(Maybe (Expr4 c))
  | E4DerefArray Location !(Expr4 c) !(Expr4 c)
  | E4DefineArray Location !(Expr4 c) !(Maybe (Expr4 c))

  | E4TypeStruct Location ![(Maybe Text, Expr4 c)]
  | E4TypeUnion Location ![(Maybe Text, Expr4 c)]

  -- Expressions while compiling
  -- structure and dereferencing
  | E4UserStructOrUnion Location !(Map Text (Expr4 c)) !(Maybe (Expr4 c))
  | E4DerefStruct Location !(Expr4 c) !Text
  -- functions
  | E4Function Location !FunctionKey ![Expr4 c]
  -- Reference to Namespace/Variable/TypeDefinition
  | E4NamespaceRef Location !Reference
  | E4Meta Location !MetaKey
  | E4LoopVariable Location !Reference
  | E4MagicValue Location !Text

deriving instance CpuData c => Eq (Expr4 c)

instance CpuData c => Show (Expr4 c) where
  show = showPrettySrc

instance CpuData c => PrettySrc (Expr4 c) where
  prettySrcM expr = do
    addFirstSourceLine (locationOf expr)
    go expr
    where
      go (E4ConstInt _ i) = return $ pretty i
      go (E4ConstMaskedInt _ i) = return $ pshow i
      go (E4ConstBool _ i) = return $ pretty i
      go E4ByteVal{} = return "!EByteVal"
      go E4CpuExpr{} = return "!ECpuExpr"
      go E4Type{} = return "!EType"
      go (E4Pointer _ l _ _)      = return $ pretty l
      go E4UserArray{} = return "!EUserArray"
      go E4UserArrayBVS{} = return "!EUserArrayBVS"
      go (E4DerefArray _ s e) = do
        s' <- prettySrcM s
        e' <- prettySrcM e
        return $ s' ++ "[" ++ e' ++ "]"
      go (E4DefineArray _ s e) = do
        s' <- prettySrcM s
        e' <- mapM prettySrcM e
        return $ s' ++ "[" ++ fromMaybe "" e' ++ "]"
      go E4TypeStruct{} = return "!ETypeStruct"
      go E4TypeUnion{} = return "!ETypeUnion"
      go E4UserStructOrUnion{} = return "!EUserStructOrUnion"
      go (E4DerefStruct _ s e) = do
        s' <- prettySrcM s
        return $ s' ++ "." ++ pretty e
      go (E4Function _ f ps)      = do
        ps' <- mapM prettySrcM ps
        let isOp = all (`elem` ("+-~!*/%++=&^|" :: String)) (functionKeyName f)
        case (isOp, ps') of
          (True, [l, r]) -> return $ "(" ++ l ++ " " ++ pshow f ++ " " ++ r ++ ")"
          (True, [r])    -> return $ "(" ++ pshow f ++ " " ++ r ++ ")"
          _              -> return $ pshow f ++ pretty '(' ++ encloseSep mempty mempty ", " ps' ++ pretty ')'
      go (E4RangedInt _ _l _h _no _s) = return "!EAddressRange"
      go (E4NamespaceRef _ n) = return $ "!E4NamespaceRef " ++ pretty n
      go (E4Meta _ n) = return $ "!E4Meta " ++ pshow n
      go (E4LoopVariable _ n) = return $ "!E4LoopVariable " ++ pretty n
      go (E4MagicValue _ v) = return $ pretty '#' ++ pretty v


instance LocationOf (Expr4 c) where
  locationOf (E4ConstInt loc _)            = loc
  locationOf (E4ConstMaskedInt loc _)      = loc
  locationOf (E4ConstBool loc _)           = loc
  locationOf (E4ByteVal loc _)             = loc
  locationOf (E4CpuExpr loc _)             = loc
  locationOf (E4Pointer loc _ _ _)         = loc
  locationOf (E4UserArray loc _ _)         = loc
  locationOf (E4UserArrayBVS loc _ _)      = loc
  locationOf (E4DerefArray loc _ _)        = loc
  locationOf (E4DefineArray loc _ _)       = loc
  locationOf (E4TypeStruct loc _)          = loc
  locationOf (E4TypeUnion loc _)           = loc
  locationOf (E4UserStructOrUnion loc _ _) = loc
  locationOf (E4DerefStruct loc _ _)       = loc
  locationOf (E4Function loc _ _)          = loc
  locationOf (E4NamespaceRef loc _)        = loc
  locationOf (E4Meta loc _)                = loc
  locationOf (E4LoopVariable loc _)        = loc
  locationOf (E4RangedInt loc _ _ _ _)     = loc
  locationOf (E4Type loc _ _)              = loc
  locationOf (E4MagicValue loc _)          = loc
