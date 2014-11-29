
{-# LANGUAGE TemplateHaskell #-}
module He.Annotation.Internal where

import Data.Lens.Template
import H.Prelude
import Text.Parsec.Applicative.Types

data Ann =
  Ann
  { _annSourcePos :: Maybe SourcePos
  } deriving (Eq, Ord, Show)

emptyAnn :: Ann
emptyAnn = Ann Nothing

makeLenses [''Ann]

