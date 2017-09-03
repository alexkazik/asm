{-# LANGUAGE CPP #-}

module Asm.Core.Flags
  ( flagDebugCompiler
  , flagDisableLocal
  ) where

import           Asm.Core.Prelude

flagDebugCompiler :: Bool
#ifdef DEBUG_COMPILER
flagDebugCompiler = True
#else
flagDebugCompiler = False
#endif

flagDisableLocal :: Bool
#ifdef DISABLE_LOCAL
flagDisableLocal = True
#else
flagDisableLocal = False
#endif
