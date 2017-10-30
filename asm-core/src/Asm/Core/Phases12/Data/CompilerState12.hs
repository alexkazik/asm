module Asm.Core.Phases12.Data.CompilerState12
  ( CompilerState12(..)
  , pushPathC
  , popPathC
  , currentPathC
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Control.CompilerError
import           Asm.Core.Data.KindDefinition
import           Asm.Core.Data.Reference
import qualified Asm.Core.Data.Tree                     as R
import           Asm.Core.Phases.Data.CompilerState1234
import           Asm.Core.SourcePos

class CompilerState1234 m => CompilerState12 m where
  getPathC :: m (Reference, [Reference], R.Tree (Location, KindDefinition))
  setPathC :: Reference -> [Reference] -> m ()

pushPathC :: CompilerState12 m => Text -> m ()
pushPathC name = do
  (csPath, csAliasPath, csData) <- getPathC
  path <- $fromJustOrError [([], "name not found")] $ R.lookup csPath name csData
  setPathC path (mkalias csAliasPath path)
  where
    mkalias csAliasPath path =
      if isPrefixOf "~" name
        then headEx csAliasPath : csAliasPath
        else path : csAliasPath

popPathC :: CompilerState12 m => m ()
popPathC = do
  (csPath, csAliasPath, csData) <- getPathC
  path <- $fromJustOrError [([], "pop root path")] $ R.parent csPath csData
  setPathC path (tailEx csAliasPath)

currentPathC :: CompilerState12 m => m Reference
currentPathC = do
  (csPath, _, _) <- getPathC
  return csPath
