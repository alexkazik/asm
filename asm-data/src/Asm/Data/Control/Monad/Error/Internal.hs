{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Asm.Data.Control.Monad.Error.Internal
  -- * The Error monad
  ( Error
  , runError
  -- * The ErrorT monad transformer
  , ErrorT(..)
  , runErrorT
  -- * Internal error type
  , ErrorType(..)
  -- * Internal helper
  , (?<>)
  , toEither
  ) where

import           Control.Monad.Trans
import           Data.Functor.Identity

import           Asm.Data.Control.Monad.Error.Class


-- The Error monad

type Error e = ErrorT e Identity

-- | The inverse of 'Error' (with the internal type hidden).
runError :: Error e a -> Either e a
{-# INLINEABLE runError #-}
runError (ErrorT m) = toEither (runIdentity m)

-- The ErrorT monad transformer

newtype ErrorT e m a = ErrorT { fromErrorT :: m (ErrorType e a) }

-- | The inverse of 'ErrorT' (with the internal type hidden).
runErrorT :: (Monad m) => ErrorT e m a -> m (Either e a)
{-# INLINEABLE runErrorT #-}
runErrorT = fmap toEither . fromErrorT

instance (Functor m) => Functor (ErrorT e m) where
  fmap f = ErrorT . fmap fmapET . fromErrorT
    where
      fmapET (Abort e)      = Abort e
      fmapET (Continue e a) = Continue e (f a)
  {-# INLINE fmap #-}

instance (Monad m, Semigroup e) => Applicative (ErrorT e m) where
  pure a = ErrorT $ return (Continue Nothing a)
  {-# INLINE pure #-}
  f <*> v = ErrorT $
    fromErrorT f >>= \case
      Abort e       -> return (Abort e)
      Continue e f' ->
        fromErrorT v >>= \case
          Abort e'       -> return (Abort (e ?<> e'))
          Continue e' v' -> return (Continue (e <> e') (f' v'))
  {-# INLINEABLE (<*>) #-}

instance (Monad m, Semigroup e) => Monad (ErrorT e m) where
  v >>= f = ErrorT $
    fromErrorT v >>= \case
      Abort e       -> return (Abort e)
      Continue e v' ->
        fromErrorT (f v') >>= \case
          Abort e'        -> return (Abort (e ?<> e'))
          Continue e' v'' -> return (Continue (e <> e') v'')
  {-# INLINEABLE (>>=) #-}

instance MonadTrans (ErrorT e) where
  lift m = ErrorT $ do
    a <- m
    return (Continue Nothing a)

-- The MonadError instance

instance (Monad m, Semigroup e) => MonadError e (ErrorT e m) where
  throwFatalError e = ErrorT $ return $ Abort e
  {-# INLINEABLE throwFatalError #-}
  throwError e = ErrorT $ return $ Continue (Just e) ()
  {-# INLINEABLE throwError #-}
  m `catchError` h = ErrorT $
    runErrorT m >>= \case
      Left e  -> fromErrorT (h e)
      Right a -> return (Continue Nothing a)
  {-# INLINEABLE catchError #-}
  m `catchFatalError` h = ErrorT $
    fromErrorT m >>= \case
      Abort e  -> fromErrorT (h e)
      continue -> return continue
  {-# INLINEABLE catchFatalError #-}

-- Internal error type

data ErrorType e a
  = Abort e
  | Continue (Maybe e) a

-- Internal helper

(?<>) :: Semigroup e => Maybe e -> e -> e
{-# INLINE (?<>) #-}
Nothing  ?<> b =      b
(Just a) ?<> b = a <> b

toEither :: ErrorType e a -> Either e a
{-# INLINE toEither #-}
toEither (Abort e)      = Left e
toEither (Continue e a) = maybe (Right a) Left e
