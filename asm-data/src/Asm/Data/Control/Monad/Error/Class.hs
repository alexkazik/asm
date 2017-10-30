{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
-- can't move this to *cabal since it will break ghci when having this in the same package as the library
{-# LANGUAGE ImplicitPrelude #-}

module Asm.Data.Control.Monad.Error.Class
  ( MonadError(..)
  ) where

import           Control.Monad.Trans
import qualified Control.Monad.Trans.RWS.Lazy   as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS


-- The MonadError class

class Monad m => MonadError e m | m -> e where
  throwFatalError :: e -> m a
  throwError :: e -> m ()
  catchError :: m a -> (e -> m a) -> m a
  catchFatalError :: m a -> (e -> m a) -> m a

instance (Monoid w, MonadError e m) => MonadError e (StrictRWS.RWST r w s m) where
  throwFatalError = lift . throwFatalError
  {-# INLINEABLE throwFatalError #-}
  throwError = lift . throwError
  {-# INLINEABLE throwError #-}
  catchError = StrictRWS.liftCatch catchError
  {-# INLINEABLE catchError #-}
  catchFatalError = StrictRWS.liftCatch catchFatalError
  {-# INLINEABLE catchFatalError #-}

instance (Monoid w, MonadError e m) => MonadError e (LazyRWS.RWST r w s m) where
  throwFatalError = lift . throwFatalError
  {-# INLINEABLE throwFatalError #-}
  throwError = lift . throwError
  {-# INLINEABLE throwError #-}
  catchError = LazyRWS.liftCatch catchError
  {-# INLINEABLE catchError #-}
  catchFatalError = LazyRWS.liftCatch catchFatalError
  {-# INLINEABLE catchFatalError #-}
