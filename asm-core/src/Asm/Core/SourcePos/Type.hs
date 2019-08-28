{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Asm.Core.SourcePos.Type
  ( SourcePos
  , Location
  , sourcePosPretty
  , sourcePosShort
  , initialSourcePos
  , sourcePosShortList
  , sourcePosShortListS
  , getPosition
  , sourceName
  , Asm.Core.SourcePos.Type.sourceLine
  , LocationOf(..)
  , spBuiltin
  ) where

import           Asm.Core.Prelude
import qualified Data.Text           as T
import qualified Language.Haskell.TH as TH
import           Text.Megaparsec     (SourcePos (..), mkPos)
import           Text.Megaparsec.Pos

type Location = [SourcePos]

sourcePosShort :: SourcePos -> Text
sourcePosShort loc = " @" ++ baseName (pack $ sourceName loc) ++ ":" ++ tshow (unPos (Text.Megaparsec.Pos.sourceLine loc))
  where
    baseName n =
      case unsnoc eidx of
        Nothing     -> n
        Just (_, l) -> l
      where
        eidx = T.split (=='/') n

sourcePosShortList :: Location -> Text
sourcePosShortList loc = "[" ++ intercalate ", " (map sourcePosShort loc) ++ "]"

sourcePosShortListS :: Location -> String
sourcePosShortListS loc = "[" ++ intercalate ", " (map (unpack . sourcePosShort) loc) ++ "]"

initialSourcePos :: String -> SourcePos
initialSourcePos = initialPos

spBuiltin :: Location
spBuiltin = [initialSourcePos "BUILTIN"]

-- helper for all quasi quoters
getPosition :: TH.Q SourcePos
getPosition = do
  loc <- TH.location
  let
    file = TH.loc_filename loc
    line = fst (TH.loc_start loc)
    col  = snd (TH.loc_start loc)
  return $ SourcePos file (mkPos $ fromIntegral line) (mkPos $ fromIntegral col)

sourceLine :: SourcePos -> Int
sourceLine = unPos . Text.Megaparsec.Pos.sourceLine

-- get location of something

class LocationOf l where
  locationOf :: l -> Location

instance LocationOf Location where
  locationOf = id

instance LocationOf l => LocationOf (a, l) where
  locationOf (_, l') = locationOf l'

instance LocationOf l => LocationOf (a, b, l) where
  locationOf (_, _, l') = locationOf l'
