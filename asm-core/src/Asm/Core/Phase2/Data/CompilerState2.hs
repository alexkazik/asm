{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Asm.Core.Phase2.Data.CompilerState2
  ( CompilerState2(..)
  , CSM2
  ) where

import           Asm.Core.Prelude
import qualified Data.Map.Strict                        as M

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

-- the state of the compiler
data CompilerState2 c
  = CSt2
    { cs2Path           :: Reference
    , cs2AliasPath      :: [Reference]
    , cs2Data           :: R.Tree (Location, KindDefinition)
    , cs2Aliases        :: Map Reference (Expr12 c)
    , cs2TypeInExpr     :: Map Reference (Expr3 c)
    , cs2PoolDefinition :: Map Reference PoolDefinition
    , cs2Position       :: Map Reference (Maybe Reference, Either (InfInt64, InfInt64) Int64)
    , cs2FunctionMap    :: FunctionKeyLookupMap
    , cs2CallPaths      :: !(Map [Text] [Expr3 c])
  }

-- the state monad it lives in
type CSM2 c = State (CompilerState2 c)

instance CpuData c => CompilerState1234S (CompilerState2 c) where
  dumpStateS s = displayPretty $ vsep
    [ "State: "
    , indent 4 $ vsep
      [ "path: " <+> pretty (cs2Path s)
      , "data:" <+> align (pretty $ cs2Data s)
      , "aliases:" <+> align (vsep $ map dumpAlias' $ M.toList $ cs2Aliases s)
      , "poolDefinition: " <+> pretty (cs2PoolDefinition s)
      , "cs2CallPaths: " <+> pshow (cs2CallPaths s)
      ]
    ]
    where
      dumpAlias' :: CpuData c => (Reference, Expr12 c) -> Doc
      dumpAlias' (i, e) = fillBreak 4 (pretty i) <+> pretty '=' <+> prettySrc e

instance CpuData c => CompilerState1234 (CSM2 c)

instance CpuData c => CompilerState12 (CSM2 c) where
  getPathC = state (\s@CSt2{..} -> ((cs2Path, cs2AliasPath, cs2Data), s))
  setPathC csPath csAliasPath = state (\s -> ((), s{cs2Path=csPath, cs2AliasPath=csAliasPath}))
