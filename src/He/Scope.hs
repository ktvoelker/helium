
module He.Scope where

import qualified Data.Map as M
import H.Common

import He.Monad

type ScopeT k v = ReaderT (Map k v)

runScopeT :: (Ord k) => ScopeT k v m a -> m a
runScopeT = flip runReaderT M.empty

findInScope :: (MonadM m, MonadReader (Map k v) m, Ord k, Show k) => k -> m v
findInScope name = join . (liftM $ maybe err return) . findInScopeMaybe $ name
  where
    err = fatal' $ Err ENotFound Nothing (Just $ show name) Nothing

findInScopeMaybe :: (MonadReader (Map k v) m, Ord k) => k -> m (Maybe v)
findInScopeMaybe = ($ ask) . liftM . M.lookup

scope :: (MonadReader (Map k v) m, Ord k) => [(k, m v)] -> m a -> m a
scope bs m = do
  bs' <- mapM f bs
  local (M.union . M.fromList $ bs') m
  where
    f (k, v) = liftM (k, ) v

scope' :: (MonadReader (Map k v) m, Ord k) => (k -> m v) -> [k] -> m a -> m a
scope' f ks = scope . zip ks . map f $ ks

