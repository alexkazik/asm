{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Asm.Data.Control.Monad.Error
  -- * The Error monad
  ( Error
  , runError
  -- * The ErrorT monad transformer
  , ErrorT
  , runErrorT
  -- * The MonadError class
  , MonadError(..)
  -- * Utility functions
  , recoverFatalError
  , promoteErrorToFatalError
  ) where

import           Asm.Data.Control.Monad.Error.Class
import           Asm.Data.Control.Monad.Error.Internal

recoverFatalError :: MonadError e m => a -> m a -> m a
{-# INLINEABLE recoverFatalError #-}
recoverFatalError def m = m `catchFatalError` (\e -> do throwError e ; return def)

promoteErrorToFatalError :: MonadError e m => m a -> m a
{-# INLINEABLE promoteErrorToFatalError #-}
promoteErrorToFatalError m = m `catchError` throwFatalError
