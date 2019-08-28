{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Asm.Core.PrettyPrint.PrettySrc
  ( PrettySrc (..)
  , prettySrc
  , showPrettySrc
  ) where

import           Asm.Core.Prelude
import qualified Data.Map.Strict                  as M
import           Data.Void
import           System.FilePath

import           Asm.Core.PrettyPrint.PPSM
import           Asm.Core.PrettyPrint.PrettyPrint as PP

class PrettySrc a where
  prettySrcM :: a -> PPSM Doc

-- produce a PP.Doc from a PrettySrc with a simple algorith to add all sources at the end
prettySrc :: PrettySrc p => p -> Doc
prettySrc x =
  let
    (d, s) = runState (prettySrcM x) (PrettyPrintState Nothing [])
    sl = M.toList $ getSimplifiedSorceLines s
    sl' = map (\(a,b) -> pshow (takeFileName a) ++ ":" ++ PP.encloseSep mempty mempty "," (map pshow (sort b))) sl
  in
    d <+> "@" ++ PP.encloseSep mempty mempty "; " sl'

-- convert PrettySrc to String (using prettySrc)
showPrettySrc :: PrettySrc p => p -> String
showPrettySrc x = show $ prettySrc x

instance PrettySrc Void where
  prettySrcM = absurd
