-- | The cardinality of a type is the number of values contained in the type.
module Cardinality where

import Data.Proxy (Proxy)

class MyEnum a where
    cardinality :: Proxy a -> Integer
    toMyEnum    :: Integer -> a
    fromMyEnum  :: a -> Integer