
{-# LANGUAGE TemplateHaskell #-}
module He.Annotation.Demo where

import H.Prelude

import He.Annotation

annotate [d|
  data Foo = A Int | B Int [Char] | C { a :: Int } | Int :+ Int
    deriving (Eq, Ord, Show) |]

