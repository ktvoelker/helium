
{-# LANGUAGE TemplateHaskell #-}
module He.RefGraph
  ( RefGraph()
  , emptyRefGraph
  , nodeLens
  , addDef
  , addRef
  , RefGraphT
  , runRefGraphT
  , addDefM
  , addRefM
  , toGraph
  , toEdgeList
  , filter
  , isCycle
  , sccs
  , SCC(..)
  , flattenSCC
  , flattenSCCs
  ) where

import Data.Graph
import Data.Lens.Template
import qualified Data.Map as M
import qualified Data.Set as S

import H.Common hiding (filter)

newtype RefGraph a = RefGraph { _rgMap :: Map a (Set a) } deriving (Eq, Ord, Show)

makeLenses [''RefGraph]

type EdgeList a = [(a, a, [a])]

emptyRefGraph :: (Ord a) => RefGraph a
emptyRefGraph = RefGraph M.empty

nodeLens :: (Ord a) => a -> Lens (RefGraph a) (Maybe (Set a))
nodeLens node = mapLens node . rgMap

addDef :: (Ord a) => a -> RefGraph a -> RefGraph a
addDef node = nodeLens node ^%= (<> Just S.empty)

addRef :: (Ord a) => a -> a -> RefGraph a -> RefGraph a
addRef from to = (nodeLens from ^%= (S.insert to <$>)) . addDef from

toGraph :: (Ord a) => RefGraph a -> Graph
toGraph = fst3 . graphFromEdges . toEdgeList

toEdgeList :: (Ord a) => RefGraph a -> EdgeList a
toEdgeList =
  map (\(k, xs) -> (k, k, S.toList xs))
  . M.toList
  . (rgMap ^$)

sccs :: (Ord a) => RefGraph a -> [SCC a]
sccs = stronglyConnComp . toEdgeList

type RefGraphT k m = StateT (RefGraph k) m

runRefGraphT :: (Ord k, Monad m) => RefGraphT k m a -> m (a, [SCC k])
runRefGraphT = liftM (\(a, rg) -> (a, sccs rg)) . flip runStateT emptyRefGraph

addDefM :: (Ord k, Monad m) => k -> RefGraphT k m ()
addDefM node = modify $ addDef node

addRefM :: (Ord k, Monad m) => k -> k -> RefGraphT k m ()
addRefM from to = modify $ addRef from to

filter :: (a -> Bool) -> RefGraph a -> RefGraph a
filter pred (RefGraph m) = RefGraph $ M.filterWithKey (flip . const $ pred) m

isCycle :: SCC a -> Bool
isCycle (AcyclicSCC _) = False
isCycle (CyclicSCC _)  = True

deriving instance (Show a) => Show (SCC a)

