{-# LANGUAGE NoImplicitPrelude #-}

module Asm.Core.Phases34.Function.Data
  ( concatArrayC
  , fillDataC
  , fillDataMetaFunctionC
  ) where

import           Asm.Core.Prelude
import qualified Data.Vector                            as V

import           Asm.Core.Data.ByteVal
import           Asm.Core.Data.Cpu
import           Asm.Core.Data.FunctionKey
import           Asm.Core.Data.KindDefinition
import           Asm.Core.Data.MetaKey
import           Asm.Core.Phase3.Data.CompilerState3
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.Phases34.Data.CompilerState34
import           Asm.Core.Phases34.Data.Function
import           Asm.Core.Phases34.Function.Check
import           Asm.Core.SourcePos


concatArrayC :: (CSM34 m, Cpu c) => Function m c
concatArrayC loc [(KDUserArray, E4UserArray _ a af), (KDUserArray, E4UserArray _ b bf)] =
  return (FnrResult (KDUserArray, E4UserArray loc (a ++ b) (af <|> bf)))
concatArrayC loc [(KDUserArray, E4UserArrayBVS _ a af), (KDUserArray, E4UserArrayBVS _ b bf)] =
  return (FnrResult (KDUserArray, E4UserArrayBVS loc (a ++ b) (af <|> bf)))
concatArrayC loc [(KDUserArray, E4UserArray _ a af), (KDUserArray, E4UserArrayBVS _ b bf)] =
  return (FnrResult (KDUserArray, E4UserArray loc (a ++ map (E4ByteVal loc . fromByteValSimple ByteValIsConst) (V.convert b)) (af <|> bf)))
concatArrayC loc [(KDUserArray, E4UserArrayBVS _ a af), (KDUserArray, E4UserArray _ b bf)] =
  return (FnrResult (KDUserArray, E4UserArray loc (map (E4ByteVal loc . fromByteValSimple ByteValIsConst) (V.convert a) ++ b) (af <|> bf)))
concatArrayC _ [(KDUserArray, _), (KDUserArray, _)] = return (FnrUnchanged KDUserArray)
concatArrayC _ _ = return FnrNoMatch

fillDataC :: (CSM34 m, Cpu c) => Function m c

fillDataC loc [(KDUserStructOrUnion, E4UserStructOrUnion _ s Nothing), (_, f)] = do
  let s' = map (\x -> E4Function loc fnFill [x, f]) s
  return (FnrResult (KDUserStructOrUnion, E4UserStructOrUnion loc s' (Just f)))
fillDataC _ [r@(KDUserStructOrUnion, E4UserStructOrUnion _ _ Just{}), _] = return (FnrResult r)
fillDataC _ [(KDUserStructOrUnion, _), _] = return (FnrUnchanged KDUserStructOrUnion)

fillDataC loc [(KDUserArray, E4UserArray _ a Nothing), (_, f)] = do
  let a' = map (\x -> E4Function loc fnFill [x, f]) a
  return (FnrResult (KDUserArray, E4UserArray loc a' (Just f)))
fillDataC _ [r@(KDUserArray, E4UserArray _ _ Just{}), _] = return (FnrResult r)
fillDataC _ [(KDUserArray, _), _] = return (FnrUnchanged KDUserArray)

fillDataC _ [ke, _] = return (FnrResult ke)
fillDataC _ _ = return FnrNoMatch

fillDataMetaFunctionC :: Cpu c => FunctionKey -> Location -> [Expr4 c] -> CSM3 c (Expr4 c)
fillDataMetaFunctionC f loc [v, fill] = do
  fn <- getCheck8C loc [metaCheckFill8, metaCheckFill, metaCheck]
  return (E4Function loc f [v, E4Function loc fn [fill]])
-- even illegal calls are just returned (will be handleld later)
fillDataMetaFunctionC f loc x = return (E4Function loc f x)
