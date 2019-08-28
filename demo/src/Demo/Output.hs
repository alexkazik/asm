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
