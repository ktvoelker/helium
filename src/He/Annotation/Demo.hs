
{-# LANGUAGE TemplateHaskell #-}
module He.Annotation.Demo where

import H.Common

import He.Annotation

annotate [d|
  data Foo = A Int | B Int String | C { a :: Int } | Int :+ Int
    deriving (Eq, Ord, Show) |]

