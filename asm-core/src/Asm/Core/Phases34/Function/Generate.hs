{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Asm.Core.Phases34.Function.Generate
  ( toAsmFunction
  , toAsmFunctionNoWild
  ) where

import           Asm.Core.Prelude
import           Language.Haskell.TH                        as TH

import           Asm.Core.Control.CompilerError
import           Asm.Core.Data.KindDefinition
import           Asm.Core.Data.Ternary
import           Asm.Core.Data.TypeDefinition
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.Phases.Data.CompilerState1234
import           Asm.Core.Phases34.Data.Function
import           Asm.Core.Phases34.Function.GenerateArgType
import           Asm.Core.SourcePos
import           Asm.Data.InfInt64

-- Application of Expressions
(...) :: Exp -> Exp -> Exp
(...) = AppE
infixr 5 ...

-- | Wrapper to convert a haskell function to a function which works within the Assembler
-- Usage:
--
-- > $(toAsmFunction 'function)
toAsmFunction
  :: Name -- ^ The name of the haskell function
  -> Q Exp -- ^ Returns a 'Q Exp' which can be used within a slice
toAsmFunction = toAsmFunction' True

toAsmFunctionNoWild
  :: Name -- ^ The name of the haskell function
  -> Q Exp -- ^ Returns a 'Q Exp' which can be used within a slice
toAsmFunctionNoWild = toAsmFunction' False


toAsmFunction'
  :: Bool
  -> Name -- ^ The name of the haskell function
  -> Q Exp -- ^ Returns a 'Q Exp' which can be used within a slice
toAsmFunction' wild fn = do
  -- extract the information about the object for which the name was passed
  typeSignature1 <- reify fn >>= \case
    (VarI _ ty _) -> return ty
    -- it's a more complex definition (e.g. with some type binding)
    x -> [printInternalError|Not a simple function: $x|]
  let
    -- check if the function is in the CompilerState1234S monad
    (typeSignature2, isMonadic) = hasCS1234Monad typeSignature1
    -- convert the argument list and map it to 'ArgType'
    typeSignature3 = map typeToAT $ argumentsToList typeSignature2
    -- extract the return type (last element)
    retT = returnAT $ unsafeLast typeSignature3
    -- remove return type from argument list
    argT1 = unsafeInit typeSignature3
    -- check if the first argument is of type Location, remember that and remove it from the argument list
    (argT, passLoc) = case argT1 of
      (ATLoc:ts) -> (ts, True)
      ts         -> (ts, False)

  -- generate names for pattern matching
  nameLoc <- newName "loc"
  nameArgs <- newName "args"

  -- generate matches for a case
  matches <- generateCase wild fn nameLoc passLoc isMonadic retT $ spliceATList argT

  -- return a lambda function which captures two arguments and apply the last to the case generated above
  -- the other argument may be referenced from within
  return
    (LamE
      [VarP nameLoc, VarP nameArgs]
      (LamCaseE matches ... VarE nameArgs)
    )

hasCS1234Monad :: TH.Type -> (TH.Type, Bool)
hasCS1234Monad (ForallT [KindedTV m1 (AppT (AppT ArrowT StarT) StarT)] [AppT (ConT st) (VarT m2)] ty) =
  if m1 == m2 && st == ''CompilerState1234
    then (removeResultMonad ty, True)
    else [printInternalError|hasCS1234Monad: Monads don't match|]
  where
    removeResultMonad (AppT (VarT m3) ty')
      | m3 == m1 = ty'
    removeResultMonad (AppT a b) = AppT a (removeResultMonad b)
    removeResultMonad _ = [printInternalError|hasCS1234Monad: Monads don't match|]
hasCS1234Monad ty = (ty, False)

-- | Convert an functions argument list ('TH.Type') into a list of 'Name' of the argment types.
--   Only really simple functions are possible to parse (no functions or types with a type [e.g. monad] as an argument)
argumentsToList :: TH.Type -> [Name]
argumentsToList (AppT (AppT ArrowT (ConT x)) y) = x : argumentsToList y
argumentsToList (ConT x)                        = [x]
argumentsToList x                               = [printInternalError|Unknown type construct: $x|]

-- | For each haskell function two case lines will be generated, one with
--   'generateCaseResult' and one with 'generateCaseUnchanged' and after that
--   one which matches everything and returns 'FnrNoMatch' to show that it's not matched.
generateCase :: Bool -> Name -> Name -> Bool -> Bool -> ArgType -> [[ArgType]] -> Q [Match]
generateCase wild fn nameLoc passLoc isMonadic ret (args:argVariations) = do
  line1 <- generateCaseResult fn nameLoc passLoc isMonadic args ret
  line2 <- generateCaseUnchanged args ret
  next <- generateCase wild fn nameLoc passLoc isMonadic ret argVariations
  if ATRangedInt `elem` args || not wild
    then return ( line1 : next )
    else return ( line1 : line2 : next )
generateCase _ _ _ _ _ _ [] =
  return [Match WildP (NormalB $ VarE 'return ... ConE 'FnrNoMatch) []]

-- | Create a pattern to match already calculated data (of the requested type),
--   pass it to the haskell function and return it's result.
generateCaseResult :: Name -> Name -> Bool -> Bool -> [ArgType] -> ArgType -> Q Match
generateCaseResult fn nameLoc passLoc isMonadic args ATRangedIntOrInt = do
  -- create a name for each argument for the pattern match
  typeNamePair <- mapM makeNameForArgument (zip [1..] args)
  resName <- newName "r"
  let
    -- Generate a pattern for each argument of type '(KDData <Type>, (_, <Const>))' where Type and Const is replaced
    -- by the corresponding type/const constructor for the argument type
    pat = map genPat typeNamePair
    -- Generate a variable for each argument, usually it's just the pattern passed though
    -- but sometimes it's a on the fly conversion (Int64 to either TInt64 or Int)
    args1 = foldr genVar [[]] typeNamePair
    -- If the haskell function requires the loc, add that to the front of the argument list
    args2 = if passLoc
      then map (VarE nameLoc :) args1
      else args1
    -- Create the call of the haskell function with all arguments
    args3 = map (foldl' AppE (VarE fn)) args2

    -- Create the body for the pattern match:
    -- Construct a new expression with the calculated data of the haskell function.
    -- Returned by 'return FnrResult x' to mark a successfully match and lift it into the monad.
    body =
      VarE 'return ...
        (VarE 'genResult ... VarE nameLoc) ...
          ListE args3
    bodyM =
      DoE
        [ BindS
            (VarP resName)
            (VarE 'sequence ... ListE args3)
        , NoBindS $
            VarE 'return ...
              (VarE 'genResult ... VarE nameLoc) ...
                VarE resName
        ]

  -- The full code out of the pattern match and function body.
  return $ Match (ListP pat) (NormalB $ bool body bodyM isMonadic) []

  where
    -- Create a new name based on "arg" and the number of the argument, returning the pair of name and 'ArgType'
    makeNameForArgument :: (Int, ArgType) -> Q (Either (Name, ArgType) (Name, Name))
    makeNameForArgument (i, ATRangedInt) = do
      nl <- newName $ "arg" ++ show i ++ "lo"
      nh <- newName $ "arg" ++ show i ++ "hi"
      return (Right (nl, nh))
    makeNameForArgument (i, e) = do
      n <- newName $ "arg" ++ show i
      return (Left (n, e))
    -- Create a pattern piece for a type
    genPat :: Either (Name, ArgType) (Name, Name) -> Pat
    genPat (Right (nl, nh)) =
      TupP
        [ ConP 'KDData [ConP 'TDInt []]
        , ConP 'E4RangedInt [WildP, VarP nl, VarP nh, WildP, WildP]
        ]
    genPat (Left (n, t)) =
      TupP
        [ ConP 'KDData [ConP (atToTDName "result/ranged/pat" t) []]
        , ConP (atToExpConstName "result/ranged/pat" t) [WildP, VarP n]
        ]
    -- create a conversion if required for a type
    genVar :: Either (Name, ArgType) (Name, Name) -> [[Exp]] -> [[Exp]]
    genVar (Left (n, ATIntAsMaskedInt)) l = map (((VarE '(*|) ... VarE n) ... LitE (IntegerL 0)) :) l
    genVar (Left (n, ATIntAsRangedInt)) l = map ((ConE 'InfInt64 ... VarE n) :) l
    genVar (Left (n, ATInt64AsInt))     l = map ((VarE 'fromIntegral ... VarE n) :) l
    genVar (Left (n, _))                l = map (VarE n :) l
    genVar (Right (nl, nh)) l             = map (VarE nl :) l ++ map (VarE nh :) l

-- | Create a pattern to match already calculated data (of the requested type),
--   pass it to the haskell function and return it's result.
-- generateCaseResult :: Name -> Name -> Bool -> [ArgType] -> ArgType -> Q Match
generateCaseResult fn nameLoc passLoc isMonadic args ret = do
  -- create a name for each argument for the pattern match
  typeNamePair <- mapM makeNameForArgument (zip [1..] args)
  resName <- newName "r"
  let
    -- Generate a pattern for each argument of type '(KDData <Type>, (_, <Const>))' where Type and Const is replaced
    -- by the corresponding type/const constructor for the argument type
    pat = map genPat typeNamePair
    -- Generate a variable for each argument, usually it's just the pattern passed though
    -- but sometimes it's a on the fly conversion (Int64 to either TInt64 or Int)
    args1 = map genVar typeNamePair
    -- If the haskell function requires the loc, add that to the front of the argument list
    args2 = if passLoc
      then VarE nameLoc : args1
      else args1
    -- Create the call of the haskell function with all arguments
    args3 = foldl' AppE (VarE fn) args2

    -- Create the body for the pattern match:
    -- Construct a new expression with the calculated data of the haskell function.
    -- Returned by 'return FnrResult x' to mark a successfully match and lift it into the monad.
    body =
      VarE 'return ...
        ConE 'FnrResult ...
          TupE
            [ ConE 'KDData ... ConE (atToTDName "result/*/ret" ret)
            , (ConE (atToExpConstName "result/*/ret" ret) ... VarE nameLoc) ... args3
            ]
    bodyM =
      ( VarE 'map ...
        LamE
          [ VarP resName ]
          ( ConE 'FnrResult ...
              TupE
                [ ConE 'KDData ... ConE (atToTDName "result/*/ret" ret)
                , (ConE (atToExpConstName "result/*/ret" ret) ... VarE nameLoc) ... VarE resName
                ]
          )
      ) ... args3

  -- The full code out of the pattern match and function body.
  return $ Match (ListP pat) (NormalB $ bool body bodyM isMonadic) []

  where
    -- Create a new name based on "arg" and the number of the argument, returning the pair of name and 'ArgType'
    makeNameForArgument :: (Int, ArgType) -> Q (Name, ArgType)
    makeNameForArgument (i, e) = do
      n <- newName $ "arg" ++ show i
      return (n, e)
    -- Create a pattern piece for a type
    genPat :: (Name, ArgType) -> Pat
    genPat (n, t) =
      TupP
        [ ConP 'KDData [ConP (atToTDName "result/*/pat" t) []]
        , ConP (atToExpConstName "result/*/pat" t) [WildP, VarP n]
        ]
    -- create a conversion if required for a type
    genVar :: (Name, ArgType) -> Exp
    genVar (n, ATIntAsMaskedInt) = (VarE '(*|) ... VarE n) ... LitE (IntegerL 0)
    genVar (n, ATInt64AsInt)     = VarE 'fromIntegral ... VarE n
    genVar (n, _)                = VarE n

-- | Create a pattern to match the correct type and create a new expression to
--   call our self again (hopefully one of the next times all data is calculated)
generateCaseUnchanged :: [ArgType] -> ArgType -> Q Match
generateCaseUnchanged args ret = do
  let
    -- Generate a pattern for each argument of type '(KDData <Type>, _)' where Type is replaced
    -- by the corresponding type constructor for the argument type, matching a wildcard
    -- for the data itself (only the type is checked).
    pat = map genPat args
    ret' = bool (atToTDName "unchanged/ret" ret) 'TDInt (ret == ATRangedIntOrInt)

    -- Create the body for the pattern match:
    -- Return that the data is not yet evaluated.
    body =
      VarE 'return ... ConE 'FnrUnchanged ... ConE 'KDData ... ConE ret'

  return $ Match (ListP pat) (NormalB body) []

  where
    -- Create a pattern piece for a type
    genPat :: ArgType -> Pat
    genPat t =
      TupP
        [ ConP 'KDData [ConP (atToTDName "unchanged/pat" t) []]
        , WildP
        ]

genResult :: Location -> [InfInt64] -> FunctionResult c
genResult loc [] = $printError [(loc, "genResult called with no arguments")]
genResult loc r =
  let
    imi@(InfInt64 mi) = minimumEx r
    ima@(InfInt64 ma) = maximumEx r
  in
    if mi /= ma
      then FnrRangedInt imi ima Nothing
      else
        if mi == minBound || mi == maxBound
          then FnrUnchanged (KDData TDInt)
          else FnrResult (KDData TDInt, E4ConstInt loc mi)
