
module He.Error where

import H.Common
import Text.Parsec.Applicative.Types

data ErrType e =
  EUnknown | EInternal | ELexer | EParser | EOutput | ENotFound | ECustom e
  deriving (Eq, Ord, Show)

data Err e =
  Err
  { errType      :: ErrType e
  , errSourcePos :: Maybe SourcePos
  , errName      :: Maybe String
  , errMore      :: Maybe String
  } deriving (Show)

