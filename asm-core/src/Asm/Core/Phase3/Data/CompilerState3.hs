{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Asm.Core.Phase3.Data.CompilerState3
  ( CompilerReader3(..)
  , CompilerWriter3(..)
  , CompilerState3(..)
  , CSM3
  ) where

import           Asm.Core.Prelude
import qualified Data.Map.Strict                        as M
import qualified Data.Set                               as S

import           Asm.Core.Control.CompilerError
import           Asm.Core.Data.CpuData
import           Asm.Core.Data.FunctionKey
import           Asm.Core.Data.KindDefinition
import           Asm.Core.Data.MetaKey
import           Asm.Core.Data.Reference
import qualified Asm.Core.Data.Tree                     as R
import           Asm.Core.Phase3.Data.Expr3
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.Phase4.Data.PoolData
import           Asm.Core.Phases.Data.CompilerState1234
import           Asm.Core.Phases.Data.PoolDefinition
import           Asm.Core.Phases34.Data.CompilerState34
import           Asm.Core.Phases34.Data.Function
import           Asm.Core.Phases34.Data.PoolState
import           Asm.Core.PrettyPrint
import           Asm.Core.SourcePos
import           Asm.Data.InfInt64

type Paths = S.Set [Text]

-- the reader of the compiler
data CompilerReader3 c
  = CRd3
    { cs3TypeInExpr     :: Map Reference (Expr3 c)
    , cs3PoolState      :: Map Reference PoolState
    , cs3PoolDefinition :: Map Reference PoolDefinition
    , cs3Functions      :: FunctionKeyMap [Function (CSM3 c) c]
    }

-- the writer of the compiler
newtype CompilerWriter3 c
  = CWr3
    { cs3Inline         :: Map Reference (Int64, Maybe (Expr4 c))
    }

instance Semigroup (CompilerWriter3 c) where
  a <> b =
    CWr3
      { cs3Inline = M.union (cs3Inline a) (cs3Inline b)
      }
instance Monoid (CompilerWriter3 c) where
  mempty =
    CWr3
      { cs3Inline = mempty
      }


-- the state of the compiler
data CompilerState3 c
  = CSt3
    { cs3Data           :: R.Tree (Location, KindDefinition)
    , cs3PoolData       :: Map Reference (PoolData c)
    , cs3MetaData       :: MetaKeyMap (KindDefinition, Expr4 c, Location)
    , cs3MetaStickyData :: MetaKeySet
    , cs3Position       :: Map Reference (Maybe Reference, Either (InfInt64, InfInt64) Int64)
    , cs3CallPaths      :: !(Map [Text] Paths)
    }

-- the monad it lives in
type CSM3 c = RWST (CompilerReader3 c) (CompilerWriter3 c) (CompilerState3 c) (Error CompilerError)

instance CpuData c => CompilerState1234S (CompilerState3 c) where
  dumpStateS s = displayPretty $ vsep
    [ "State: "
    , indent 4 $ vsep
      [ "cs3Data:" <+> align (pretty $ cs3Data s)
      , "cs3PoolData: " <+> align (pretty $ cs3PoolData s)
      -- , "cs3MetaData: " <+> pretty (cs3MetaData s)
      -- , "cs3MetaStickyData: " <+> pretty (cs3MetaStickyData s)
      , "cs3Position: " <+> pshow (cs3Position s)
      , "cs3CallPaths: " <+> pretty (cs3CallPaths s)
      ]
    ]

instance CpuData c => CompilerState1234 (CSM3 c)

instance CpuData c => CSM34 (CSM3 c) where
  -- function
  type CSM34Cpu (CSM3 c) = c
  lookupFunctionC k = asks (fromMaybe [] . fkmLookup k . cs3Functions)
  -- pool
  toolPoolGetPoolStateC = asks cs3PoolState
  toolPoolGetPoolDefinitionC = asks cs3PoolDefinition
  -- position
  toolPositionGetC = gets cs3Position
  setPositionC n v = modify (\s -> s{cs3Position = M.insert n v (cs3Position s)})
  isPhase4C = return False
