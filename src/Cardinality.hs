module Cardinality where

import Data.Proxy (Proxy)

class MyEnum a where
    cardinality :: Proxy a -> Integer
    toMyEnum    :: Integer -> a
    fromMyEnum  :: a -> Integer