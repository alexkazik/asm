{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Asm.Core.Phase2.Data.CompilerState2
  ( CompilerReader2(..)
  , CompilerWriter2(..)
  , CompilerState2(..)
  , CSM2
  ) where

import           Asm.Core.Prelude
import qualified Data.Map.Strict                        as M

import           Asm.Core.Control.CompilerError
import           Asm.Core.Data.CpuData
import           Asm.Core.Data.FunctionKey
import           Asm.Core.Data.KindDefinition
import           Asm.Core.Data.Reference
import qualified Asm.Core.Data.Tree                     as R
import           Asm.Core.Phase1.Data.Expr12
import           Asm.Core.Phase3.Data.Expr3
import           Asm.Core.Phases.Data.CompilerState1234
import           Asm.Core.Phases.Data.PoolDefinition
import           Asm.Core.Phases12.Data.CompilerState12
import           Asm.Core.PrettyPrint
import           Asm.Core.SourcePos
import           Asm.Data.InfInt64

-- the reader of the compiler
data CompilerReader2 c
  = CRd2
    { cs2Aliases        :: Map Reference (Expr12 c)
    , cs2PoolDefinition :: Map Reference PoolDefinition
    , cs2FunctionMap    :: FunctionKeyLookupMap
    }

-- the writer of the compiler
data CompilerWriter2 c
  = CWr2
    { cs2Position  :: Map Reference (Maybe Reference, Either (InfInt64, InfInt64) Int64)
    , cs2CallPaths :: !(Map [Text] [Expr3 c])
    }

instance Semigroup (CompilerWriter2 c) where
  a <> b =
    CWr2
      { cs2Position = M.union (cs2Position a) (cs2Position b)
      , cs2CallPaths = M.unionWith (<>) (cs2CallPaths a) (cs2CallPaths b)
      }

instance Monoid (CompilerWriter2 c) where
  mempty =
    CWr2
      { cs2Position = mempty
      , cs2CallPaths = mempty
      }

-- the state of the compiler
data CompilerState2 c
  = CSt2
    { cs2Path       :: Reference
    , cs2AliasPath  :: [Reference]
    , cs2Data       :: R.Tree (Location, KindDefinition)
    , cs2TypeInExpr :: Map Reference (Expr3 c)
    }

-- the monad it lives in
type CSM2 c = RWST (CompilerReader2 c) (CompilerWriter2 c) (CompilerState2 c) (Error CompilerError)

instance CpuData c => CompilerState1234S (CompilerState2 c) where
  dumpStateS s = displayPretty $ vsep
    [ "State: "
    , indent 4 $ vsep
      [ "path: " <+> pretty (cs2Path s)
      , "data:" <+> align (pretty $ cs2Data s)
      ]
    ]

instance CpuData c => CompilerState1234 (CSM2 c)

instance CpuData c => CompilerState12 (CSM2 c) where
  getPathC = gets (\CSt2{..} -> (cs2Path, cs2AliasPath, cs2Data))
  setPathC csPath csAliasPath = modify (\s -> s{cs2Path=csPath, cs2AliasPath=csAliasPath})
