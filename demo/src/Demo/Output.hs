-- can't move this to *cabal since it will break ghci when having this in the same package as the library
{-# LANGUAGE ImplicitPrelude #-}

module Demo.Output
  ( ModuleOutput(..)
  , moduleOutput
  ) where

import           Data.ByteString.Builder

import           Asm.Core.Compiler


data ModuleOutput
  = ModuleOutput
    { moName  :: String
    , moStats :: String
    , moFiles :: [(FilePath, Builder)]
    }

moduleOutput :: String -> CompilerResult c -> [(FilePath, Builder)] -> ModuleOutput
moduleOutput moduleName compilerResult files =
  ModuleOutput
    { moName = moduleName
    , moStats = show (crPoolsStats compilerResult)
    , moFiles = files
    }
