{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Asm.Cpu6502.OpCodes.Generator
  ( AddressMode(..)
  , IndexMode(..)
  , mkOpCodes
  ) where

import           Asm.Core.Prelude
import           Data.Generics.Aliases        (extQ)
import qualified Data.IntMap.Strict           as IM
import qualified Data.Map.Strict              as M
import qualified Data.Text                    as T
import qualified Language.Haskell.TH          as TH
import qualified Language.Haskell.TH.Syntax   as TH

import           Asm.Core.PrettyPrint         (pstring)
import           Asm.Core.PrettyPrint.Use
import           Asm.Parser.Quote             (liftText)

import           Asm.Cpu6502.Data.FunctionKey
import           Asm.Cpu6502.OpCodes.Parser


mkOpCodes :: String -> [(String, String)] -> TH.Q [TH.Dec]
mkOpCodes source aliases = do
  -- Internal: parse input
  let
    (cpuMap, operatorList) = parseOpcodes source
  -- newtype CpuVariant
  nameCpuVariantType <- TH.newName "CpuVariant"
  nameCpuVariantCon <- TH.newName "CpuVariant"
  nameCpuVariantRecord <- TH.newName "fromCpuVariant"
  let
    decCpuVariant =
      TH.NewtypeD
        []
        nameCpuVariantType
        []
        Nothing
        (TH.RecC nameCpuVariantCon [(nameCpuVariantRecord, TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, TH.ConT ''Int)])
#if MIN_VERSION_template_haskell(2,12,0)
        [TH.DerivClause Nothing
#endif
        [TH.ConT ''Eq, TH.ConT ''Data, TH.ConT ''Typeable, TH.ConT ''Show]
#if MIN_VERSION_template_haskell(2,12,0)
        ]
#endif
  -- cpuVariants :: Map Text CpuVariant
  nameCpuVariants <- TH.newName "cpuVariants"
  typeCpuVariants <- [t|Map Text $(TH.conT nameCpuVariantType)|]
  let
    toExpCpuVariants (k, v) = TH.TupE [ TH.LitE $ TH.StringL (T.unpack k), TH.AppE (TH.ConE nameCpuVariantCon) (TH.LitE $ TH.IntegerL $ fromIntegral v) ]
    expCpuVariants = map toExpCpuVariants $ M.toList cpuMap
  let
    sigCpuVariants =
      TH.SigD
        nameCpuVariants
        typeCpuVariants
    decCpuVariants =
      TH.FunD
        nameCpuVariants
        [ TH.Clause
            []
            (TH.NormalB $ TH.AppE (TH.VarE 'M.fromList) (TH.ListE expCpuVariants))
            []
        ]
  -- cpuVariantXXX :: CpuVariant
  namesIntCpuVariants <- forM (M.toList cpuMap) $ \(n, c) -> do
    opn <- TH.newName $ "cpuVariant" ++ unpack n
    return (opn, c)
  let
    decCpuVariantXXX = flip map namesIntCpuVariants $ \(fn, c) ->
      [ TH.SigD fn $ TH.ConT nameCpuVariantType
      , TH.FunD
          fn
          [ TH.Clause
              []
              (TH.NormalB $ TH.AppE (TH.ConE nameCpuVariantCon) (TH.LitE $ TH.IntegerL $ fromIntegral c))
              []
          ]
      ]
  -- Internal: generate a name for each operator as "oprXXX"
  let
    namesOprFn' = M.fromList $ map (\(n, _, _, c, _, _, _) -> (n, c)) operatorList
    aliasOprFn' = mapMaybe (\(a, o) -> map ((,) a) $ M.lookup o namesOprFn') aliases
  namesOprFn <- forM (M.toList namesOprFn' ++ aliasOprFn') $ \(n, c) -> do
    opn <- TH.newName $ "opr" ++ toUpper n
    return (n, c, opn)
  -- newtype Operator
  nameOperatorType <- TH.newName "Operator"
  nameOperatorCon <- TH.newName "Operator"
  nameOperatorRecord <- TH.newName "fromOperator"
  let
    decOperator =
      TH.NewtypeD
        []
        nameOperatorType
        []
        Nothing
        (TH.RecC nameOperatorCon [(nameOperatorRecord, TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, TH.ConT ''Int)])
#if MIN_VERSION_template_haskell(2,12,0)
        [TH.DerivClause Nothing
#endif
        [TH.ConT ''Eq, TH.ConT ''Data, TH.ConT ''Typeable, TH.ConT ''Show]
#if MIN_VERSION_template_haskell(2,12,0)
        ]
#endif
  -- instance Pretty Operator
  let
    clausesOperatorPrettyFn = flip map namesOprFn $ \(n, c, _) ->
      TH.Clause
        [TH.ConP nameOperatorCon [TH.LitP $ TH.IntegerL $ fromIntegral c]]
        (TH.NormalB $ TH.AppE (TH.VarE 'pstring) (TH.LitE $ TH.StringL n))
        []
  let
    decOperatorPrettyInstance =
      TH.InstanceD
        Nothing
        []
        (TH.AppT (TH.ConT ''Pretty) (TH.ConT nameOperatorType))
        [TH.FunD 'pretty clausesOperatorPrettyFn]
  -- oprXXX :: Operator
  let
    decOprXXX = flip map namesOprFn $ \(_, c, fn) ->
      [ TH.SigD fn $ TH.ConT nameOperatorType
      , TH.FunD
          fn
          [ TH.Clause
              []
              (TH.NormalB $ TH.AppE (TH.ConE nameOperatorCon) (TH.LitE $ TH.IntegerL $ fromIntegral c))
              []
          ]
      ]
  -- opcodeNames :: [(Text, Operator)]
  nameOpcodeNames <- TH.newName "opcodeNames"
  typeOpcodeNames <- [t|[(Text, $(TH.conT nameOperatorType))]|]
  let
    expOperatorNames = flip map namesOprFn $ \(n, _, fn) ->
      TH.TupE
        [ TH.LitE $ TH.StringL n
        , TH.VarE fn
        ]
  let
    sigOpcodeNames =
      TH.SigD
        nameOpcodeNames
        typeOpcodeNames
    decOpcodeNames =
      TH.FunD
        nameOpcodeNames
        [ TH.Clause
            []
            (TH.NormalB $ TH.ListE expOperatorNames)
            []
        ]
  -- opcodes :: IntMap (IntMap (Map IndexMode (Map AddressMode (Word8, Maybe FunctionKey, Maybe FunctionKey))))
  nameOpcodes <- TH.newName "opcodes"
  typeOpcodes <- [t|IntMap (IntMap (Map IndexMode (Map AddressMode (Word8, Maybe FunctionKey, Maybe FunctionKey))))|]
  let
    addOp
      :: (String, AddressMode, IndexMode, Word8, [Int], Maybe FunctionKey, Maybe FunctionKey)
      -> IntMap (IntMap (Map IndexMode (Map AddressMode (Word8, Maybe FunctionKey, Maybe FunctionKey))))
      -> IntMap (IntMap (Map IndexMode (Map AddressMode (Word8, Maybe FunctionKey, Maybe FunctionKey))))
    addOp (n, am, im, c, cpus, check, addr) os =
      let
        op = IM.singleton (fromIntegral $ namesOprFn' M.! n) $ M.singleton im $ M.singleton am (c, check, addr)
        addCpu cpu = IM.unionWith (IM.unionWith (M.unionWith M.union)) (IM.singleton cpu op)
      in
        foldr addCpu os cpus
  ops <- liftData $ foldr addOp IM.empty operatorList
  let
    sigOpcodes =
      TH.SigD
        nameOpcodes
        typeOpcodes
    decOpcodes =
      TH.FunD
        nameOpcodes
        [ TH.Clause
            []
            (TH.NormalB ops)
            []
        ]
  -- all together
  return $
    [ decCpuVariant
    , sigCpuVariants
    , decCpuVariants
    , decOperator
    , decOperatorPrettyInstance
    , sigOpcodeNames
    , decOpcodeNames
    , sigOpcodes
    , decOpcodes
    ]
    ++ concat decCpuVariantXXX
    ++ concat decOprXXX

liftData :: Data a => a -> TH.ExpQ
liftData = TH.dataToExpQ (const Nothing `extQ` liftText)
