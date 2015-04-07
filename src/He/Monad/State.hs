
{-# LANGUAGE TemplateHaskell #-}
module He.Monad.State where

import Control.Lens
import qualified Data.Text.IO as TIO
import H.IO
import H.Prelude

newtype PrimId = PrimId { primName :: Text } deriving (Eq, Ord, Show)

primId :: Text -> PrimId
primId = PrimId

data Unique = Prim PrimId | Unique Integer Text deriving (Show)

primUnique :: PrimId -> Unique
primUnique = Prim

uniquePrim :: Unique -> Maybe (PrimId)
uniquePrim (Prim id) = Just id
uniquePrim (Unique _ _) = Nothing

uniqueSourceName :: Unique -> Text
uniqueSourceName (Unique _ xs) = xs
uniqueSourceName (Prim id) = primName id

class FromPrimId a where
  fromPrimId :: PrimId -> a

instance FromPrimId Unique where
  fromPrimId = primUnique

instance Eq Unique where
  (==) (Prim xs) (Prim ys) = xs == ys
  (==) (Unique m _) (Unique n _) = m == n
  (==) (Prim _) (Unique _ _) = False
  (==) (Unique _ _) (Prim _) = False

instance Ord Unique where
  compare (Prim xs) (Prim ys) = compare xs ys
  compare (Unique m _) (Unique n _) = compare m n
  compare (Prim _) (Unique _ _) = LT
  compare (Unique _ _) (Prim _) = GT

data MTState =
  MTState
  { _mtNextUnique :: Integer
  , _mtErrorCount :: Integer
  , _mtLogger     :: Text -> IO ()
  }

emptyMTState :: MTState
emptyMTState = MTState 0 0 logStdErr

logStdErr :: Text -> IO ()
logStdErr = TIO.putStrLn

makeLenses ''MTState

