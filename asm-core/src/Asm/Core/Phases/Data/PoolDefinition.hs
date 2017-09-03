module Asm.Core.Phases.Data.PoolDefinition
  ( PoolDefinition(..)
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Data.Reference
import           Asm.Core.PrettyPrint

data PoolDefinition
  = PoolDefinition
    { pdBank    :: !Int64
    , pdStart   :: !Int64
    , pdVirtual :: !Bool
    , pdPools   :: ![Reference]
    }
  deriving (Eq)

instance Pretty PoolDefinition where
  pretty PoolDefinition{pdBank, pdStart, pdVirtual, pdPools} = list
    [ encloseSep lparen rparen (pstring "=") [ pstring "bank", pretty pdBank ]
    , encloseSep lparen rparen (pstring "=") [ pstring "start", pretty pdStart ]
    , encloseSep lparen rparen (pstring "=") [ pstring "virtual", pretty pdVirtual ]
    , encloseSep lparen rparen (pstring "=") [ pstring "pools", pretty pdPools ]
    ]
