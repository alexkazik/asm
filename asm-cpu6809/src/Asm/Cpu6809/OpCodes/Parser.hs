{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Asm.Cpu6809.OpCodes.Parser
  ( AddressMode(..)
  , parseOpcodes
  ) where

import           Asm.Core.Prelude
import           Data.List                    (mapAccumR)
import           Data.List.Extra              (split)
import qualified Data.Map.Strict              as M
import qualified Data.Text                    as T
import           Numeric

import           Asm.Core.PrettyPrint.Use

import           Asm.Cpu6809.Data.FunctionKey

data AddressMode
  = AMImm
  | AMDir
  | AMIdx
  | AMExt
  | AMImp
  | AMRel
  deriving (Typeable,Data,Eq,Ord)

instance Show AddressMode where
  show AMImm = "imm"
  show AMDir = "dir"
  show AMIdx = "idx"
  show AMExt = "ext"
  show AMImp = "imp"
  show AMRel = "rel"

instance Pretty AddressMode where
  pretty = pshow

instance Pretty [AddressMode] where
  pretty = prettyList


parseLine :: Map Text Int -> [String] -> (Map Text Int, (String, AddressMode, Bool, Word16, [Int], Maybe FunctionKey))
parseLine cpuMap (op:am:co:cp:mty) =
  let
    cpusText = T.split (== ',') $ T.dropWhile (== '[') $ T.dropWhileEnd (== ']') $ pack cp
    (cpuMap', cpus) = mapAccumR addCpu cpuMap cpusText
  in
  ( cpuMap'
  , ( op
    , toam am
    , is16 am
    , readHex' co
    , cpus
    , cty mty
    )
  )
  where
    cty :: [String] -> Maybe FunctionKey
    cty []       = Nothing
    cty ["byte"] = Just fnAddrByte
    cty ["code"] = Just fnAddrCode
    cty x        = error $ "unknown check type: " ++ show x

    readHex' :: String -> Word16
    readHex' a = fst $ headEx $ filter (null . snd) $ readHex a

    toam :: String -> AddressMode
    toam "imm8"  = AMImm
    toam "imm16" = AMImm
    toam "dir"   = AMDir
    toam "idx"   = AMIdx
    toam "ext"   = AMExt
    toam "imp"   = AMImp
    toam "rel8"  = AMRel
    toam "rel16" = AMRel
    toam x       = error $ "unknown am: " ++ x

    is16 :: String -> Bool
    is16 "imm16" = True
    is16 "rel16" = True
    is16 _       = False

    addCpu :: Map Text Int -> Text -> (Map Text Int, Int)
    addCpu cpuMap'' cpu =
      case M.lookup cpu cpuMap of
        Just cpuNr -> (cpuMap'', cpuNr)
        Nothing ->
          let
            cpuNr = M.size cpuMap''
          in
            (M.insert cpu cpuNr cpuMap'', cpuNr)
parseLine _ x = error $ "invalid line: " ++ show x

parseOpcodes :: String -> (Map Text Int, [(String, AddressMode, Bool, Word16, [Int], Maybe FunctionKey)])
parseOpcodes source =
  let
    splitLine = filter (not . null) . split (\x -> x == ' ' || x == '.')
    removeComment = fst . break (== '#')
    sourceLines = filter (not . null) $ map (splitLine . removeComment) $ split (== '\n') source
  in
    mapAccumR parseLine M.empty sourceLines
