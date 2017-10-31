module Asm.Core.Data.CompilerError
  ( CompilerError(..)
  , convertCompilerError
  , convertCompilerErrorWithoutState
  , printCompilerError
  ) where

import           Asm.Core.Prelude
import           System.IO.Unsafe

import           Asm.Core.Flags
import           Asm.Core.SourcePos.Type

newtype CompilerError
  = CompilerError
    { fromCompilerError :: ([(Location, String)], Maybe LText)
    }

instance Semigroup CompilerError where
  CompilerError (ae, as) <> CompilerError (be, bs) = CompilerError (ae ++ be, bs <|> as)

convertCompilerError :: CompilerError -> [(String, [String])]
convertCompilerError (CompilerError (errs, Just sta))
  | flagDebugCompiler = unsafePerformIO $ do
      writeFile "/tmp/asm.sta" (encodeUtf8 (toStrict sta) ++ "\n")
      return (convertCompilerErrorWithoutState errs)
convertCompilerError (CompilerError (errs, _)) =
  convertCompilerErrorWithoutState errs

convertCompilerErrorWithoutState :: [(Location, String)] -> [(String, [String])]
convertCompilerErrorWithoutState =
  map (\(loc, err) -> (err, map sourcePosPretty loc))

printCompilerError :: [(String, [String])] -> a
printCompilerError err =
  errorWithoutStackTrace $ "Error: " ++
  concatMap (\(msg, loc) ->
    msg ++ "\n" ++ concatMap (\x -> "- " ++ x ++ "\n") loc
  ) err
