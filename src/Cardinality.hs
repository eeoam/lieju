{-| The cardinality of a type is the number of values contained in the type.
-}
module Cardinality (
    -- * Version 0
    -- $version0

    -- * Version 1
    -- $version1
    GMyEnum(..)
    ) where

import Data.Proxy 
    ( Proxy (..)
    )

import GHC.Generics
    ( (:+:) (..)
    )

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

instance (GMyEnum a, GMyEnum b) => GMyEnum (a :+: b) where
    gcardinality _ = gcardinality (Proxy @a) + gcardinality (Proxy @b)

    toGMyEnum n | n < cardA = L1 (toGMyEnum n)
                | otherwise = R1 (toGMyEnum (n - cardA))
        where cardA = gcardinality (Proxy @a)

    fromGMyEnum = \case
        L1 x -> fromGMyEnum x
        R1 x -> fromGMyEnum x + gcardinality (Proxy @a)