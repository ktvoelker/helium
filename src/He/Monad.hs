
module He.Monad
  ( ErrType(..)
  , Err(..)
  , fatal
  , fatal'
  , report
  , report'
  , check
  , checked
  , internal
  , log
  , Unique()
  , uniquePrim
  , uniqueSourceName
  , nextUnique
  , PrimId()
  , primName
  , primId
  , primUnique
  , FromPrimId(..)
  , MT()
  , MonadM(..)
  , runMT
  , execMT
  ) where

import Control.Exception
import H.IO
import H.Prelude
import Data.Typeable

import He.Error
import He.Monad.State
import He.Monad.Types

data Abort = Abort
  deriving (Eq, Ord, Enum, Bounded, Show, Typeable)

instance Exception Abort where

getErrorCount :: (MonadM m) => m Integer
getErrorCount = liftMT . MT $ gets _mtErrorCount

abort :: (MonadM m) => m a
abort = do
  getErrorCount >>= \c -> log $ "Aborted with " <> show c <> " errors"
  liftIO $ throwIO Abort

incrErrorCount :: (MonadM m) => m ()
incrErrorCount = void . liftMT . MT $ mtErrorCount %= (+ 1)

fatal :: (MonadM m, Show e) => Err e -> m a
fatal e = report e >> abort

fatal' :: (MonadM m) => Err () -> m a
fatal' = fatal

report :: (MonadM m, Show e) => Err e -> m ()
report e = incrErrorCount >> log (show e)

report' :: (MonadM m) => Err () -> m ()
report' = report

check :: (MonadM m) => m ()
check = getErrorCount >>= \case
  0 -> return ()
  _ -> abort

checked :: (MonadM m) => m a -> m a
checked = (<* check)

internal :: (MonadM m, Show a) => a -> m b
internal = fatal' . Err EInternal Nothing Nothing . Just . show

log :: (MonadM m) => Text -> m ()
log xs = liftMT . MT $ gets _mtLogger >>= liftIO . ($ xs)

class (Functor m, Applicative m, MonadIO m) => MonadM m where
  liftMT :: MT a -> m a

instance MonadM MT where
  liftMT = id

instance (MonadM m) => MonadM (ReaderT r m) where
  liftMT = lift . liftMT

nextUnique :: (MonadM m) => Text -> m Unique
nextUnique name =
  liftMT . MT $ Unique <$> (mtNextUnique %%= \u -> (u, u + 1)) <*> pure name

runMT :: MT a -> IO a
runMT = flip evalStateT emptyMTState . unMT

execMT :: MT a -> IO ()
execMT = void . runMT

