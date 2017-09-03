{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Asm.Cpu6502.OpCodes.Parser
  ( AddressMode(..)
  , IndexMode(..)
  , parseOpcodes
  ) where

import           Asm.Core.Prelude
import           Data.List                    (mapAccumR)
import           Data.List.Extra              (split)
import qualified Data.Map.Strict              as M
import qualified Data.Text                    as T
import           Numeric

import           Asm.Core.PrettyPrint.Use

import           Asm.Cpu6502.Data.FunctionKey

data AddressMode
  = AMAbs
  | AMZp
  | AMImm
  | AMImp
  | AMRel
  deriving (Typeable,Data,Show,Eq,Ord)

instance Pretty AddressMode where
  pretty = pshow

instance Pretty [AddressMode] where
  pretty = prettyList


data IndexMode
  = IMNone
  | IMX
  | IMY
  | IMIY
  | IMXI
  | IMI
  deriving (Typeable,Data,Show,Eq,Ord)

instance Pretty IndexMode where
  pretty = pshow

instance Pretty [IndexMode] where
  pretty = prettyList


parseLine :: Map Text Int -> [String] -> (Map Text Int, (String, AddressMode, IndexMode, Word8, [Int], Maybe FunctionKey, Maybe FunctionKey))
parseLine cpuMap (op:amim:co:cp:mty) =
  let
    cpusText = T.split (== ',') $ T.dropWhile (== '[') $ T.dropWhileEnd (== ']') $ pack cp
    (cpuMap', cpus) = mapAccumR addCpu cpuMap cpusText
    (am, im) = toam amim
    (afn, cfn) = cty mty
  in
  ( cpuMap'
  , ( op
    , am
    , im
    , readHex' co
    , cpus
    , cfn
    , afn
    )
  )
  where
    cty :: [String] -> (Maybe FunctionKey, Maybe FunctionKey)
    cty []           = (Nothing, Nothing)
    cty ["-"]        = (Nothing, Nothing)
    cty ["byte"]     = (Just fnAddrByte, Nothing)
    cty ["code"]     = (Just fnAddrCode, Nothing)
    cty ["-", fn]    = (Nothing, Just $ fk fn)
    cty ["byte", fn] = (Just fnAddrByte, Just $ fk fn)
    cty ["code", fn] = (Just fnAddrCode, Just $ fk fn)
    cty x            = error $ "unknown check type: " ++ show x

    fk "fnCheckLaxImm" = fnCheckLaxImm
    fk "fnCheckJmpInd" = fnCheckJmpInd
    fk x               = error $ "unknown function: " ++ show x

    readHex' :: String -> Word8
    readHex' a = fst $ headEx $ filter (null . snd) $ readHex a

    toam :: String -> (AddressMode, IndexMode)
    toam "abs"    = (AMAbs, IMNone)
    toam "abs-x"  = (AMAbs, IMX)
    toam "abs-y"  = (AMAbs, IMY)
    toam "abs-xi" = (AMAbs, IMXI)
    toam "abs-iy" = (AMAbs, IMIY)
    toam "abs-i"  = (AMAbs, IMI)
    toam "zp"     = (AMZp, IMNone)
    toam "zp-x"   = (AMZp, IMX)
    toam "zp-y"   = (AMZp, IMY)
    toam "zp-xi"  = (AMZp, IMXI)
    toam "zp-iy"  = (AMZp, IMIY)
    toam "imm"    = (AMImm, IMNone)
    toam "imp"    = (AMImp, IMNone)
    toam "rel"    = (AMRel, IMNone)
    toam x        = error $ "unknown am/im: " ++ x

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

parseOpcodes :: String -> (Map Text Int, [(String, AddressMode, IndexMode, Word8, [Int], Maybe FunctionKey, Maybe FunctionKey)])
parseOpcodes source =
  let
    splitLine = filter (not . null) . split (\x -> x == ' ' || x == '.')
    removeComment = fst . break (== '#')
    sourceLines = filter (not . null) $ map (splitLine . removeComment) $ split (== '\n') source
  in
    mapAccumR parseLine M.empty sourceLines
