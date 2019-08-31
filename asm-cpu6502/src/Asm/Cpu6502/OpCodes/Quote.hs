{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Asm.Cpu6502.OpCodes.Quote
  ( opc
  ) where

import           Asm.Core.Prelude
import qualified Data.IntMap.Strict        as IM
import           Data.List                 (nub)
import           Data.List.Extra           (split)
import qualified Data.Map.Strict           as M
import           Data.Tuple.Extra          (fst3)
import qualified Language.Haskell.TH       as TH
import           Language.Haskell.TH.Quote (QuasiQuoter (..))

import           Asm.Cpu6502.Data.OpCodes

opc :: QuasiQuoter
opc = QuasiQuoter
  { quoteExp = quoteOpCode TH.LitE
  , quotePat = quoteOpCode TH.LitP
  , quoteDec = error "QuasiQuoter \"opc\" for declarations is not available"
  , quoteType = error "QuasiQuoter \"opc\" for types is not available"
  }

quoteOpCode :: (TH.Lit -> a) -> String -> TH.Q a
quoteOpCode lit s = do
  let
    (cpu', op', am') =
      case split (\x -> x == '.' || x == ':') s of
        [op, am]      -> (Nothing, op, am)
        [op, am, cpu] -> (Just $ pack cpu, op, am)
        _             -> error "can't detect opcode.address-mode[:cpu]"
  let
    cpu = map (\cpu'' -> fromMaybe (error "unknown cpu") $ M.lookup cpu'' cpuVariants) cpu'
    op = fromMaybe (error "unknown op") $ lookup (pack op') opcodeNames
    (am,im) =
      case am' of
        "abs"   -> (AMAbs, IMNone)
        "abs-x" -> (AMAbs, IMX)
        "abs-y" -> (AMAbs, IMY)
        "abs-i" -> (AMAbs, IMI)
        "zp"    -> (AMZp, IMNone)
        "zp-x"  -> (AMZp, IMX)
        "zp-y"  -> (AMZp, IMY)
        "zp-xi" -> (AMZp, IMXI)
        "zp-iy" -> (AMZp, IMIY)
        "imm"   -> (AMImm, IMNone)
        "imp"   -> (AMImp, IMNone)
        "rel"   -> (AMRel, IMNone)
        _       -> error "unknown address/index-mode"
    oc1s =
      maybe
        (IM.elems opcodes)
        (\cpu'' -> [opcodes IM.! fromCpuVariant cpu'']) -- gurenteed due to Text->CpuVariant lookup above
        cpu
    oc2s = mapMaybe (IM.lookup (fromOperator op)) oc1s
    oc3s = mapMaybe (M.lookup im) oc2s
    oc4s = mapMaybe (M.lookup am) oc3s
    codes = map fst3 oc4s
  case nub codes of
    [code] -> return (lit $ TH.IntegerL $ fromIntegral code)
    []     -> bool (error "unknown address-mode/index-mode for op") (error "op not found") (null oc2s)
    _      -> error "different opcodes found, specify cpu"
