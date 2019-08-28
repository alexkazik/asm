{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Asm.Parser.Parser.Integer
  ( parseInteger
  , parseIntExprI
  ) where

import           Asm.Core.Prelude
import           Data.Char               (digitToInt)
import           Text.Read               (read)

import           Asm.Core.Data.Ternary

import           Asm.Parser.Data.PExpr
import           Asm.Parser.Parser.Basic

--
-- integer parser (with support for 0[oxbd]..)
--

parseInteger :: Parser Integer
parseInteger =
  choice
    [ (string' "0o" *> octal)       <?> "octal integer"
    , (string' "0x" *> hexadecimal) <?> "hexadecimal integer"
    , (string' "0b" *> binary)      <?> "binary integer"
    , (string' "0d" *> decimal)     <?> "decimal integer"
    , decimal                       <?> "decimal integer"
    ] <?> "integer"
  where
    binary :: Parser Integer
    binary = do
      num <- someOf "01._#" <?> "binary digit"
      return $ toDec 1 1 num

    octal :: Parser Integer
    octal = do
      num <- someOf "01234567._#" <?> "octal digit"
      return $ toDec 3 7 num

    hexadecimal :: Parser Integer
    hexadecimal = do
      num <- someOf' "0123456789abcdefABCDEF._#" <?> "hexadecimal digit"
      return $ toDec 4 15 num

    decimal :: Parser Integer
    decimal = do
      num <- someOf "0123456789." <?> "decimal digit"
      return $ read $ filter (/= '.') $ unpack num

    -- common
    addChar :: Int -> Integer -> Integer -> Char -> Integer
    addChar sft _    acc '_' = acc `shiftL` sft
    addChar sft full acc '#' = acc `shiftL` sft + full
    addChar sft _    acc  c  = acc `shiftL` sft + fromIntegral (digitToInt c)

    toDec :: Int -> Integer -> Text -> Integer
    toDec sft full s = foldl' (addChar sft full) 0 (filter (/= '.') $ unpack s)


-- parses a Int64 / TInt64
-- overflow is discarded silently

parseIntExprI :: Parser (PExprI pe)
parseIntExprI =
  choice
    [ (string' "0o" *> (checkMask <$> octal))       <?> "octal integer"
    , (string' "0x" *> (checkMask <$> hexadecimal)) <?> "hexadecimal integer"
    , (string' "0b" *> (checkMask <$> binary))      <?> "binary integer"
    , (string' "0d" *> decimal)                     <?> "decimal integer"
    , decimal                                       <?> "decimal integer"
    ] <?> "integer"
  where
    binary :: Parser TInt64
    binary = do
      num <- someOf "01._#?" <?> "binary digit"
      return $ toDec 1 1 num

    octal :: Parser TInt64
    octal = do
      num <- someOf "01234567._#?" <?> "octal digit"
      return $ toDec 3 7 num

    hexadecimal :: Parser TInt64
    hexadecimal = do
      num <- someOf' "0123456789abcdef._#?" <?> "hexadecimal digit"
      return $ toDec 4 15 num

    decimal :: Parser (PExprI pe)
    decimal = do
      num <- someOf "0123456789." <?> "decimal digit"
      return $ PEConstInt $ read $ filter (/= '.') $ unpack num

    -- common
    addChar :: Int -> Int64 -> TInt64 -> Char -> TInt64
    addChar sft full mi '?' = (mi `shiftL` sft) *- full
    addChar sft _    mi '_' =  mi `shiftL` sft
    addChar sft full mi '#' = (mi `shiftL` sft) *+> (full *& full)
    addChar sft full mi  c  = (mi `shiftL` sft) *+> (fromIntegral (digitToInt c) *& full)

    toDec :: Int -> Int64 -> Text -> TInt64
    toDec sft full s = foldl' (addChar sft full) (if "?" `isPrefixOf` reals then unknown else zeroBits) reals
      where
        reals = filter ('.'/=) $ unpack s

    checkMask :: TInt64 -> PExprI pe
    checkMask mi
      | m == -1 = PEConstInt v
      | otherwise = PEConstMaskedInt mi
      where
        (v,m) = tValueFillAndMask 0 mi
