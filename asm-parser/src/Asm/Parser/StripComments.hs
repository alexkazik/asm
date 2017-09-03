module Asm.Parser.StripComments
  ( stripComments
  ) where

import           Asm.Core.Prelude
import qualified Data.Text.Lazy          as LT

import           Asm.Parser.Parser.Basic

data Mode = ModeNone | ModeLine | ModeBlockC | ModeBlockHaskell

isWhiteSpace :: Char -> Bool
isWhiteSpace = flip elem whiteSpaceWithNewline

parseComment :: Mode -> String -> LText
-- empty input -> done (all modes)
parseComment _ ""                         = ""
-- when in mode none
parseComment ModeNone ('/':'/':s)         = "  " ++ parseComment ModeLine s
parseComment ModeNone ('-':'-':s)         = "  " ++ parseComment ModeLine s
parseComment ModeNone ('/':'*':s)         = "  " ++ parseComment ModeBlockC s
parseComment ModeNone ('{':'-':s)         = "  " ++ parseComment ModeBlockHaskell s
parseComment ModeNone (c:s)               = c `cons` parseComment ModeNone s
-- how to end an mode line
parseComment ModeLine ('\n':s)            = '\n' `cons` parseComment ModeNone s
-- how to end an mode block
parseComment ModeBlockC ('*':'/':s)       = "  " ++ parseComment ModeNone s
parseComment ModeBlockHaskell ('-':'}':s) = "  " ++ parseComment ModeNone s
-- while within mode line or block -> replace all non whitespace chars with a space
parseComment mode (c:s)                   = (if isWhiteSpace c then c else ' ') `cons` parseComment mode s


stripComments :: String -> Text
stripComments s = toStrict $ LT.dropWhileEnd isWhiteSpace $ parseComment ModeNone s
