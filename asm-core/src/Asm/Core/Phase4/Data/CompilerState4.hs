{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Asm.Core.Phase4.Data.CompilerState4
  ( CompilerState4(..)
  , CSM4
  ) where

import           Asm.Core.Prelude
import qualified Data.Map.Strict                        as M
import qualified Data.Set                               as S

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

type Paths = S.Set [Text]

-- the state of the compiler
data CompilerState4 c
  = CSt4
    { cs4UniqueNumber   :: !Int
    , cs4PoolData       :: Map Reference (PoolData c)
    , cs4PoolState      :: Map Reference PoolState
    , cs4PoolDefinition :: Map Reference PoolDefinition
    , cs4HighestDefault :: Maybe (Ratio Int)
    , cs4UseDefault     :: Maybe (Ratio Int)
    , cs4HasChanged     :: !Bool
    , cs4MetaIsFlat     :: !Bool
    , cs4Position       :: Map Reference (Maybe Reference, Either (InfInt64, InfInt64) Int64)
    , cs4Inline         :: Map Reference (Int64, Maybe (Expr4 c))
    , cs4Functions      :: FunctionKeyMap [Function (CSM4 c) c]
    , cs4CallPaths      :: !(Map [Text] Paths)
    }

instance CpuData c => Eq (CompilerState4 c) where
  a == b =
    cs4UniqueNumber a == cs4UniqueNumber b &&
    cs4PoolData a == cs4PoolData b &&
    cs4PoolState a == cs4PoolState b &&
    cs4PoolDefinition a == cs4PoolDefinition b &&
    cs4HighestDefault a == cs4HighestDefault b &&
    cs4UseDefault a == cs4UseDefault b &&
    cs4HasChanged a == cs4HasChanged b &&
    cs4MetaIsFlat a == cs4MetaIsFlat b &&
    cs4Position a == cs4Position b &&
    cs4Inline a == cs4Inline b

-- the state monad it lives in
type CSM4 c = State (CompilerState4 c)

instance CpuData c => CompilerState1234S (CompilerState4 c) where
  dumpStateS s = displayPretty $ vsep
    [ "State: "
    , indent 4 $ vsep
      [ "cs4UniqueNumber: " <+> pretty (show $ cs4UniqueNumber s)
      , "poolData: " <+> align (pretty $ cs4PoolData s)
      , "poolState: " <+> list (map dumpPoolState $ M.toList $ cs4PoolState s)
      , "poolDefinition: " <+> pretty (cs4PoolDefinition s)
      , "cs4UseDefault: " <+> pretty (show $ cs4UseDefault s)
      , "cs4Position: " <+> pretty (show $ cs4Position s)
      , "cs4Inline: " <+> pretty (show $ cs4Inline s)
      , "cs4CallPaths: " <+> pretty (cs4CallPaths s)
      , "??cs4MetaIsFlat: " <+> pretty (show $ cs4MetaIsFlat s)
      , "??cs4HighestDefault: " <+> pretty (show $ cs4HighestDefault s)
      , "??cs4HasChanged: " <+> pretty (show $ cs4HasChanged s)
      ]
    ]

instance CpuData c => CompilerState1234 (CSM4 c) where
  setHasChangedC = state (\s -> ((), s{cs4HasChanged = True}))

instance CpuData c => CSM34 (CSM4 c) where
  -- function
  type CSM34Cpu (CSM4 c) = c
  lookupFunctionC k = state (\s -> (fromMaybe [] $ fkmLookup k (cs4Functions s), s))
  -- pool
  toolPoolGetPoolStateC = state (\s -> (cs4PoolState s, s))
  toolPoolGetPoolDefinitionC = state (\s -> (cs4PoolDefinition s, s))
  -- position
  toolPositionGetC = state (\s -> (cs4Position s, s))
  setPositionC n v = state (\s -> ((), s{cs4Position = M.insert n v (cs4Position s)}))
  isPhase4C = return True
