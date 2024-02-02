{-| The cardinality of a type is the number of values contained in the type.
-}
module Cardinality (
    -- * Version 0
    -- $version0

    -- * Version 1
    -- $version1
    GMyEnum(..)
    ) where

import Data.Proxy (Proxy)

{- $version0 
@
class MyEnum a where
    'cardinality' :: 'Proxy' a -> 'Integer'
    'toMyEnum'    :: 'Integer' -> a
    'fromMyEnum'  :: a -> 'Integer'
@
-}


{- $version1
-}
class GMyEnum f where
    gcardinality :: Proxy f -> Integer
    toGMyEnum :: Integer -> f a
    fromGMyEnum :: f a -> Integer