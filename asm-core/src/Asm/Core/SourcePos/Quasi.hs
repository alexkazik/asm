module Asm.Core.SourcePos.Quasi
  ( sourcePos
  ) where

import           Asm.Core.Prelude
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (liftData)

import           Asm.Core.Flags
import           Asm.Core.SourcePos.Type

-- external definition
sourcePos :: Q Exp
sourcePos
  | flagDebugCompiler = do
      pos <- getPosition
      [e|[($(liftData [pos]), "SOURCE")]|]
  | otherwise = [e|[]|]
