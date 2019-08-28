{-# LANGUAGE NoImplicitPrelude #-}

module Asm.Core.Phases34.Data.PoolState
  ( PoolState(..)
  , emptyPoolState
  , dumpPoolState
  ) where

import           Asm.Core.Prelude

import           Asm.Core.PrettyPrint
import           Asm.Data.InfInt64

data PoolState
  = PoolState
    { psStartLow   :: !Int64
    , psStartHigh  :: !InfInt64
    , psLengthLow  :: !Int64
    , psLengthHigh :: !InfInt64
    }
  deriving (Eq, Show)

instance Pretty PoolState where
  pretty = pshow

emptyPoolState :: PoolState
emptyPoolState = PoolState {psStartLow=0, psStartHigh=maxBound, psLengthLow=0, psLengthHigh=maxBound}

dumpPoolState :: Pretty n => (n, PoolState) -> Doc
dumpPoolState (n, s) = fillBreak 12 (pretty n) <+> pretty '=' <+> pshow s
