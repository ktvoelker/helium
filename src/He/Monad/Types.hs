
module He.Monad.Types where

import H.Common

import He.Monad.State

newtype MT a = MT { unMT :: StateT MTState IO a }

instance Functor MT where
  fmap f = MT . fmap f . unMT

instance Applicative MT where
  pure = MT . pure
  (<*>) f x = MT $ unMT f <*> unMT x

instance Monad MT where
  (>>=) (MT m) f = MT $ m >>= unMT . f
  return = MT . return
  fail = MT . fail

instance MonadIO MT where
  liftIO = MT . liftIO

