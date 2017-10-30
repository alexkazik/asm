{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Asm.Core.Phase1.Data.CompilerState1
  ( CompilerWriter1(..)
  , CompilerState1(..)
  , CSM1
  ) where

import           Asm.Core.Prelude
import qualified Data.Map.Strict                        as M

import           Asm.Core.Control.CompilerError
import           Asm.Core.Data.CpuData
import           Asm.Core.Data.KindDefinition
import           Asm.Core.Data.Reference
import qualified Asm.Core.Data.Tree                     as R
import           Asm.Core.Phase1.Data.Expr12
import           Asm.Core.Phases.Data.CompilerState1234
import           Asm.Core.Phases.Data.PoolDefinition
import           Asm.Core.Phases12.Data.CompilerState12
import           Asm.Core.PrettyPrint
import           Asm.Core.SourcePos

-- the writer of the compiler
data CompilerWriter1 c =
  CWr1
    { cs1Aliases        :: Map Reference (Expr12 c)
    , cs1PoolDefinition :: Map Reference PoolDefinition
    }

instance Monoid (CompilerWriter1 c) where
  mempty =
    CWr1
      { cs1Aliases = mempty
      , cs1PoolDefinition = mempty
      }
  a `mappend` b =
    CWr1
      { cs1Aliases = M.union (cs1Aliases a) (cs1Aliases b)
      , cs1PoolDefinition = M.union (cs1PoolDefinition a) (cs1PoolDefinition b)
      }

-- the state of the compiler
data CompilerState1 c =
  CSt1
    { cs1Path            :: Reference
    , cs1AliasPath       :: [Reference]
    , cs1Data            :: R.Tree (Location, KindDefinition)
    , cs1UniqueNumber    :: !Int
    , cs1OnlySuperLocals :: !Int
    , cs1OnlySystemNames :: !Int
    }

-- the monad it lives in
type CSM1 c = RWST () (CompilerWriter1 c) (CompilerState1 c) (Error CompilerError)

instance CpuData c => CompilerState1234S (CompilerState1 c) where
  dumpStateS s = displayPretty $ vsep
    [ "State: "
    , indent 4 $ vsep
      [ "path: " <+> pretty (cs1Path s)
      , "data:" <+> align (pretty $ cs1Data s)
      , "uniqueNumber: " <+> pretty (cs1UniqueNumber s)
      , "onlySuperLocals: " <+> pretty (cs1OnlySuperLocals s)
      ]
    ]

instance CpuData c => CompilerState1234 (CSM1 c)

instance CpuData c => CompilerState12 (CSM1 c) where
  getPathC = gets (\CSt1{..} -> (cs1Path, cs1AliasPath, cs1Data))
  setPathC csPath csAliasPath = modify (\s -> s{cs1Path=csPath, cs1AliasPath=csAliasPath})
