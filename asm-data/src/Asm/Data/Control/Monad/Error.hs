{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
-- can't move this to *cabal since it will break ghci when having this in the same package as the library
{-# LANGUAGE ImplicitPrelude #-}

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
