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
    , type (+)
    , type (*)
    )

-- package finite-typelits
import Data.Finite
    ( Finite
    , shiftN
    , weakenN
    , separateSum
    , combineProduct
    , separateProduct
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
    toGMyEnum _ = error "GMyEnum: V1 has no inhabitants"
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
@
class Enume a where
    type Cardinality a :: 'Nat'
    toEnume   :: 'Finite' (Cardinality a) -> a
    fromEnume :: a -> 'Finite' (Cardinality a)
@
-}

class (KnownNat (GCardinality f)) => GEnume (f :: Type -> Type) where
    type GCardinality f :: Nat
    gToEnume   :: Finite (GCardinality f) -> f a
    gFromEnume :: f a -> Finite (GCardinality f)


instance (GEnume a) => GEnume (M1 _x _y a) where
    type GCardinality (M1 _x _y a) = GCardinality a
    gToEnume = M1 . gToEnume
    gFromEnume = gFromEnume . unM1
    

instance GEnume V1 where
    type GCardinality V1 = 0
    gToEnume   _ = error "GEnume: V1 has no inhabitants"
    gFromEnume _ = error "GEnume: V1 has no inhabitants"
    

instance GEnume U1 where
    type GCardinality U1 = 1
    gToEnume = const U1
    gFromEnume   = const 0

{-Does not compile 
instance (Enume a) => GEnume (K1 _x a) where
    type GCardinality (K1 _x a) = Cardinality a

    gToEnume   :: Finite (GCardinality f) -> f a
    gToEnume = K1 . gToEnume

    gFromEnume :: f a -> Finite (GCardinality f)
    gFromEnume = gFromEnume . unK1
-}


instance (GEnume a, GEnume b) => GEnume (a :+: b) where
    type GCardinality (a :+: b) = GCardinality a + GCardinality b

    gToEnume = either (L1 . gToEnume) (R1 . gToEnume) . separateSum

    gFromEnume (L1 x) = weakenN . gFromEnume $ x
    gFromEnume (R1 y) = shiftN  . gFromEnume $ y

instance (GEnume a, GEnume b) => GEnume (a :*: b) where
    type GCardinality (a :*: b) = GCardinality a * GCardinality b

    gToEnume xy = let
        (x,y) = separateProduct xy
        in gToEnume x :*: gToEnume y

    gFromEnume (x :*: y) = 
        combineProduct  @(GCardinality a) @(GCardinality b) (weakenN $ gFromEnume x, weakenN $ gFromEnume y)

{-





-}

class (Eq a, KnownNat (Cardinality a)) => Enume (a :: Type) where
    type Cardinality a :: Nat
    type Cardinality a = GCardinality (Rep a)

    -- | Converts an index to its corresponding inhabitant. 
    toEnume   :: Finite (Cardinality a) -> a
    default toEnume :: (Generic a, GEnume (Rep a), Cardinality a ~ GCardinality (Rep a))
                => Finite (Cardinality a) -> a
    toEnume = to . gToEnume

    -- | Converts an inhabitant to its corresponding index.
    fromEnume :: a -> Finite (Cardinality a)
    default fromEnume :: (Generic a, GEnume (Rep a), Cardinality a ~ GCardinality (Rep a))
                      => a -> Finite (Cardinality a)
    fromEnume = gFromEnume . from

--instance Enume Void
--instance Enume ()
--instance Enume Bool