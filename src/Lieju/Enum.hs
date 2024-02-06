{-| The cardinality of a type is the number of values contained in the type.
lièjǔ is pinyin for to list/enumerate
-}
module Lieju.Enum (
    -- * Version 0
    -- $version0
      MyEnum(..)
    , GMyEnum(..)

    -- * Version 1
    -- $version1
    , Enume(..)
    , GEnume(..)
    ) where

import Data.Proxy 
    ( Proxy (..)
    )

import GHC.Generics
    ( Generic
    , V1
    , U1(..)
    , K1(..)
    , M1(..)
    , (:+:) (..)
    , (:*:) (..)
    , Rep
    , from
    , to
    )

import Data.Kind ( Type )

import Data.Void ( Void )

import GHC.TypeNats
    ( Nat
    , KnownNat(..)
    )

-- package finite-typelits
import Data.Finite
    ( Finite
    )

{- $version0 
@
class MyEnum a where
    'cardinality' :: 'Proxy' a -> 'Integer'
    'toMyEnum'    :: 'Integer' -> a
    'fromMyEnum'  :: a -> 'Integer'
@
-}
class MyEnum (a :: Type) where
    cardinality :: Proxy a -> Integer
    default cardinality :: (Generic a, GMyEnum (Rep a)) => Proxy a -> Integer
    cardinality _ = gcardinality (Proxy :: Proxy (Rep a))

    

    toMyEnum :: Integer -> a
    default toMyEnum :: (Generic a, GMyEnum (Rep a)) => Integer -> a
    toMyEnum = to . toGMyEnum

    fromMyEnum :: a -> Integer
    default fromMyEnum :: (Generic a, GMyEnum (Rep a)) => a -> Integer
    fromMyEnum = fromGMyEnum . from

instance MyEnum Void
instance MyEnum ()
instance MyEnum Bool

class GMyEnum f where
    gcardinality :: Proxy f -> Integer
    toGMyEnum :: Integer -> f a
    fromGMyEnum :: f a -> Integer

instance GMyEnum V1 where
    gcardinality _ = 0
    toGMyEnum _ = error "GFinite: V1 has no inhabitants"
    fromGMyEnum _ = error "GMyEnum: V1 has no inhabitants"

instance GMyEnum U1 where
    gcardinality _ = 1    
    toGMyEnum = const U1
    fromGMyEnum = const 0

instance (GMyEnum a) => GMyEnum (M1 _x _y a) where
    gcardinality _ = gcardinality (Proxy @a)
    toGMyEnum = M1 . toGMyEnum
    fromGMyEnum = fromGMyEnum . unM1

instance (MyEnum a) => GMyEnum (K1 _x a) where
    gcardinality _ = cardinality (Proxy @a)
    toGMyEnum = K1 . toMyEnum
    fromGMyEnum = fromMyEnum . unK1

instance (GMyEnum a, GMyEnum b) => GMyEnum (a :+: b) where
    gcardinality _ = gcardinality (Proxy @a) + gcardinality (Proxy @b)

    toGMyEnum n | n < cardA = L1 (toGMyEnum n)
                | otherwise = R1 (toGMyEnum (n - cardA))
        where cardA = gcardinality (Proxy @a)

    fromGMyEnum = \case
        L1 x -> fromGMyEnum x
        R1 x -> fromGMyEnum x + gcardinality (Proxy @a)

instance (GMyEnum a, GMyEnum b) => GMyEnum (a :*: b) where
    gcardinality _ = gcardinality (Proxy @a) * gcardinality (Proxy @b)

    toGMyEnum n = toGMyEnum q :*: toGMyEnum r
        where
            cardB = gcardinality (Proxy @b)
            (q, r) = n `quotRem` cardB

    fromGMyEnum (q :*: r) =
        gcardinality (Proxy @b) * fromGMyEnum q + fromGMyEnum r


{- $version1
-}
class Enume a where
    type Cardinality a :: Nat
    toEnume   :: Finite (Cardinality a) -> a
    fromEnume :: a -> Finite(Cardinality a)

class (KnownNat (GCardinality f)) => GEnume (f :: Type -> Type) where
    type GCardinality f :: Nat
    gToEnume :: f a -> Finite (GCardinality f)
    gFromEnume :: Finite (GCardinality f) -> f x

