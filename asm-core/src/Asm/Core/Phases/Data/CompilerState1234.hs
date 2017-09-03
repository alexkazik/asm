module Asm.Core.Phases.Data.CompilerState1234
  ( CompilerState1234S(..)
  , printErrorS
  , CompilerState1234(..)
  , printErrorC
  , fromMaybeC
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Flags
import           Asm.Core.SourcePos


class CompilerState1234S s where
  dumpStateS :: s -> LText

-- 1, 2, 3, 4
class Monad m => CompilerState1234 m where
  dumpStateC :: m LText
  default dumpStateC :: (MonadState s m, CompilerState1234S s) => m LText
  dumpStateC = dumpStateS <$> get
  setHasChangedC :: m ()
  setHasChangedC = return ()

printErrorD :: [(Location, String)] -> LText -> a
printErrorD err ds =
  seq ds $ printError $ err ++ bool [] [([], unpack ds)] flagDebugCompiler

printErrorS :: (CompilerState1234S s) => s -> [(Location, String)] -> a
printErrorS s err = printErrorD err (dumpStateS s)

printErrorC :: (CompilerState1234 m) => [(Location, String)] -> m a
printErrorC err = printErrorD err =<< dumpStateC

fromMaybeC :: (CompilerState1234 m) => [(Location, String)] -> Maybe a -> m a
fromMaybeC errs Nothing = printErrorC $ errs++[sourcePos||]
fromMaybeC _ (Just x)   = return x
