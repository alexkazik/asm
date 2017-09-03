-- can't move this to *cabal since it will break ghci when having this in the same package as the library
{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Demo.Output where

import           Asm.Core.Compiler


moduleOutput :: String -> CompilerResult c -> String
moduleOutput moduleName compilerResult =
    "Module: " ++ moduleName ++ "\n" ++
    show (crPoolsStats compilerResult) ++ "\n"
