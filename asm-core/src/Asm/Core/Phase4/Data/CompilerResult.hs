{-# LANGUAGE NoImplicitPrelude #-}

module Asm.Core.Phase4.Data.CompilerResult
  ( CompilerResult(..)
  , crPoolsWithData
  , crPoolsStats
  , crDumpState
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Data.Cpu
import           Asm.Core.Phase4.CompilerState4
import           Asm.Data.ByteValSimple

data CompilerResult c
  = CompilerResult
    { crPools :: [(Text, Int64, Int64, Maybe (Vector ByteValSimple))]
    , crState :: CompilerState4 c
    }

crPoolsWithData :: CompilerResult c -> [(Text, (Int64, Vector ByteValSimple))]
crPoolsWithData cr = mapMaybe go (crPools cr)
  where
    go (_, _, _, Nothing) = Nothing
    go (n, s, _, Just d)  = Just (n, (s, d))

crPoolsStats :: CompilerResult c -> [(Text, (Int64, Int64, Bool))]
crPoolsStats cr = map (\(n, s, l, d) -> (n, (s, l, isNothing d))) (crPools cr)

crDumpState :: Cpu c => CompilerResult c -> LText
crDumpState cr = dumpStateS (crState cr)
