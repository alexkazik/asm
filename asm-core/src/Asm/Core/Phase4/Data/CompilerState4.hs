{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Asm.Core.Phase4.Data.CompilerState4
  ( CompilerReader4(..)
  , CompilerState4(..)
  , CSM4
  ) where

import           Asm.Core.Prelude
import qualified Data.Map.Strict                        as M

import           Asm.Core.Control.CompilerError
import           Asm.Core.Data.CpuData
import           Asm.Core.Data.FunctionKey
import           Asm.Core.Data.Reference
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.Phase4.Data.PoolData
import           Asm.Core.Phases.Data.CompilerState1234
import           Asm.Core.Phases.Data.PoolDefinition
import           Asm.Core.Phases34.Data.CompilerState34
import           Asm.Core.Phases34.Data.Function
import           Asm.Core.Phases34.Data.PoolState
import           Asm.Core.PrettyPrint
import           Asm.Data.InfInt64

-- the reader of the compiler
data CompilerReader4 c
  = CRd4
    { cs4PoolDefinition :: Map Reference PoolDefinition
    , cs4Functions      :: FunctionKeyMap [Function (CSM4 c) c]
    }

-- the state of the compiler
data CompilerState4 c
  = CSt4
    { cs4UniqueNumber   :: !Int
    , cs4PoolData       :: Map Reference (PoolData c)
    , cs4PoolState      :: Map Reference PoolState
    , cs4HighestDefault :: Maybe (Ratio Int)
    , cs4UseDefault     :: Maybe (Ratio Int)
    , cs4HasChanged     :: !Bool
    , cs4MetaIsFlat     :: !Bool
    , cs4Position       :: Map Reference (Maybe Reference, Either (InfInt64, InfInt64) Int64)
    , cs4Inline         :: Map Reference (Int64, Maybe (Expr4 c))
    }

-- the monad it lives in
type CSM4 c = RWST (CompilerReader4 c) () (CompilerState4 c) (Error CompilerError)

instance CpuData c => Eq (CompilerState4 c) where
  a == b =
    cs4UniqueNumber a == cs4UniqueNumber b &&
    cs4PoolData a == cs4PoolData b &&
    cs4PoolState a == cs4PoolState b &&
    cs4HighestDefault a == cs4HighestDefault b &&
    cs4UseDefault a == cs4UseDefault b &&
    cs4HasChanged a == cs4HasChanged b &&
    cs4MetaIsFlat a == cs4MetaIsFlat b &&
    cs4Position a == cs4Position b &&
    cs4Inline a == cs4Inline b

instance CpuData c => CompilerState1234S (CompilerState4 c) where
  dumpStateS s = displayPretty $ vsep
    [ "State: "
    , indent 4 $ vsep
      [ "cs4UniqueNumber: " <+> pretty (show $ cs4UniqueNumber s)
      , "poolData: " <+> align (pretty $ cs4PoolData s)
      , "poolState: " <+> list (map dumpPoolState $ M.toList $ cs4PoolState s)
      , "cs4UseDefault: " <+> pretty (show $ cs4UseDefault s)
      , "cs4Position: " <+> pretty (show $ cs4Position s)
      , "cs4Inline: " <+> pretty (show $ cs4Inline s)
      , "??cs4MetaIsFlat: " <+> pretty (show $ cs4MetaIsFlat s)
      , "??cs4HighestDefault: " <+> pretty (show $ cs4HighestDefault s)
      , "??cs4HasChanged: " <+> pretty (show $ cs4HasChanged s)
      ]
    ]

instance CpuData c => CompilerState1234 (CSM4 c) where
  setHasChangedC = modify (\s -> s{cs4HasChanged = True})

instance CpuData c => CSM34 (CSM4 c) where
  -- function
  type CSM34Cpu (CSM4 c) = c
  lookupFunctionC k = asks (fromMaybe [] . fkmLookup k . cs4Functions)
  -- pool
  toolPoolGetPoolStateC = gets cs4PoolState
  toolPoolGetPoolDefinitionC = asks cs4PoolDefinition
  -- position
  toolPositionGetC = gets cs4Position
  setPositionC n v = modify go
    where
      go s
        | Just v == M.lookup n (cs4Position s) = s
        | otherwise = s{cs4Position = M.insert n v (cs4Position s), cs4HasChanged = True}
  isPhase4C = return True
