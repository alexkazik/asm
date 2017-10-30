module Asm.Core.Data.CompilerError
  ( CompilerError(..)
  , convertCompilerError
  , convertCompilerErrorWithoutState
  , printCompilerError
  ) where

import           Asm.Core.Prelude

import           Asm.Core.SourcePos.Type

newtype CompilerError
  = CompilerError
    { fromCompilerError :: ([(Location, String)], Maybe LText)
    }

instance Semigroup CompilerError where
  CompilerError (ae, as) <> CompilerError (be, bs) = CompilerError (ae ++ be, maybe as Just bs)

convertCompilerError :: CompilerError -> [(String, [String])]
convertCompilerError (CompilerError (errs, sta)) =
  convertCompilerErrorWithoutState $ errs ++ maybe [] (\l -> [([], unpack l)]) sta

convertCompilerErrorWithoutState :: [(Location, String)] -> [(String, [String])]
convertCompilerErrorWithoutState errs =
  map (\(loc, err) -> (err, map sourcePosPretty loc)) errs

printCompilerError :: [(String, [String])] -> a
printCompilerError err =
  errorWithoutStackTrace $ "Error: " ++
  concatMap (\(msg, loc) ->
    msg ++ "\n" ++ concatMap (\x -> "- " ++ x ++ "\n") loc
  ) err
