{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Asm.Core.PrettyPrint.PrettyPrint
  ( module PP
  , Doc
  , displayPretty
  , fillList
  , fillListNoSpace
  , pshow
  , pstring
  , (<|+|>)
  , (<|-|>)
  ) where

import           Asm.Core.Prelude
import           Data.Text.Prettyprint.Doc                 hiding (Doc, Pretty (..))
import qualified Data.Text.Prettyprint.Doc                 as PP
import           Data.Text.Prettyprint.Doc.Render.Terminal
import qualified System.Console.Terminal.Size              as Con
import           System.IO.Unsafe

type Doc = PP.Doc AnsiStyle

fillList :: [Doc] -> Doc
fillList (x:xs) = foldl' (\z y -> z <|-|> ", " ++ y) ("[ " ++ x) xs <|-|> "]"
fillList []     = "[]"

fillListNoSpace :: [Doc] -> Doc
fillListNoSpace (x:xs) = foldl' (\z y -> z <|-|> "," ++ y) ("[" ++ x) xs <|-|> "]"
fillListNoSpace []     = "[]"

pshow :: Show a => a -> Doc
pshow = PP.pretty . tshow

pstring :: Text -> Doc
pstring = PP.pretty

displayPretty :: Doc -> LText
displayPretty doc = renderLazy $ layoutPretty (LayoutOptions $ AvailablePerLine outputWidth ribbonWidth) doc

ribbonWidth :: Double
ribbonWidth = 0.6

outputWidthDefault :: Int
outputWidthDefault = 120

outputWidth :: Int
outputWidth = fromMaybe outputWidthDefault (map Con.width (unsafePerformIO Con.size))

(<|+|>) :: Doc -> Doc -> Doc
(<|+|>) a b = a ++ softline ++ b
infixr 6 <|+|>

(<|-|>) :: Doc -> Doc -> Doc
(<|-|>) a b = a ++ softline' ++ b
infixr 6 <|-|>
