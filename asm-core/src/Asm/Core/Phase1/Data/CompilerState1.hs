{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Asm.Core.Phase1.Data.CompilerState1
  ( CompilerState1(..)
  , CSM1
  ) where

import           Asm.Core.Prelude
import qualified Data.Map.Strict                        as M

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

-- the state of the compiler
data CompilerState1 c =
  CSt1
    { cs1Path            :: Reference
    , cs1AliasPath       :: [Reference]
    , cs1Data            :: R.Tree (Location, KindDefinition)
    , cs1Aliases         :: Map Reference (Expr12 c)
    , cs1UniqueNumber    :: !Int
    , cs1OnlySuperLocals :: !Int
    , cs1OnlySystemNames :: !Int
    , cs1PoolDefinition  :: Map Reference PoolDefinition
    }

-- the state monad it lives in
type CSM1 c = State (CompilerState1 c)

instance CpuData c => CompilerState1234S (CompilerState1 c) where
  dumpStateS s = displayPretty $ vsep
    [ "State: "
    , indent 4 $ vsep
      [ "path: " <+> pretty (cs1Path s)
      , "data:" <+> align (pretty $ cs1Data s)
      , "aliases:" <+> align (vsep $ map dumpAlias' $ M.toList $ cs1Aliases s)
      , "uniqueNumber: " <+> pretty (cs1UniqueNumber s)
      , "onlySuperLocals: " <+> pretty (cs1OnlySuperLocals s)
      , "poolDefinition: " <+> pretty (cs1PoolDefinition s)
      ]
    ]
    where
      dumpAlias' :: CpuData c => (Reference, Expr12 c) -> Doc
      dumpAlias' (i, e) = fillBreak 4 (pretty i) <+> pretty '=' <+> prettySrc e

instance CpuData c => CompilerState1234 (CSM1 c)

instance CpuData c => CompilerState12 (CSM1 c) where
  getPathC = state (\s@CSt1{..} -> ((cs1Path, cs1AliasPath, cs1Data), s))
  setPathC csPath csAliasPath = state (\s -> ((), s{cs1Path=csPath, cs1AliasPath=csAliasPath}))
