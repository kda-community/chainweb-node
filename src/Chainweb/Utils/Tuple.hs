{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Tuple helpers and strict tuples.
module Chainweb.Utils.Tuple
( thd, T2(..), T3(..), T4(..), sfst, ssnd, scurry, suncurry
, suncurry3, uncurry3, _T2, _T3
) where

import Control.DeepSeq (NFData)
import Control.Lens (Field1(..), Field2(..), Field3(..), Field4(..), Iso, iso, lens)
import Data.Bifunctor (Bifunctor(..))
import GHC.Generics (Generic)

-- -------------------------------------------------------------------------- --
-- Tuples

thd :: (a,b,c) -> c
thd (_,_,c) = c
{-# INLINE thd #-}

-- -------------------------------------------------------------------------- --
-- Strict Tuple

data T2 a b = T2 !a !b
    deriving (Show, Eq, Ord, Generic, NFData, Functor, Foldable, Traversable)

instance (Semigroup a, Semigroup b) => Semigroup (T2 a b) where
    T2 a b <> T2 a' b' = T2 (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (T2 a b) where
    mappend = (<>)
    mempty = T2 mempty mempty

instance Bifunctor T2 where
    bimap f g (T2 a b) =  T2 (f a) (g b)
    {-# INLINE bimap #-}
instance Field1 (T2 a b) (T2 x b) a x where
    _1 = lens (\(T2 a _b) -> a) (\(T2 _a b) x -> T2 x b)
instance Field2 (T2 a b) (T2 a x) b x where
    _2 = lens (\(T2 _a b) -> b) (\(T2 a _b) x -> T2 a x)

data T3 a b c = T3 !a !b !c
    deriving (Show, Eq, Ord, Generic, NFData, Functor)

instance Bifunctor (T3 a) where
    bimap f g (T3 a b c) =  T3 a (f b) (g c)
    {-# INLINE bimap #-}
instance Field1 (T3 a b c) (T3 x b c) a x where
    _1 = lens (\(T3 a _b _c) -> a) (\(T3 _a b c) x -> T3 x b c)
instance Field2 (T3 a b c) (T3 a x c) b x where
    _2 = lens (\(T3 _a b _c) -> b) (\(T3 a _b c) x -> T3 a x c)
instance Field3 (T3 a b c) (T3 a b x) c x where
    _3 = lens (\(T3 _a _b c) -> c) (\(T3 a b _c) x -> T3 a b x)

data T4 a b c d = T4 !a !b !c !d
    deriving (Show, Eq, Ord, Generic, NFData, Functor)

instance Field1 (T4 a b c d) (T4 x b c d) a x where
    _1 = lens (\(T4 a _b _c _d) -> a) (\(T4 _a b c d) x -> T4 x b c d)
instance Field2 (T4 a b c d) (T4 a x c d) b x where
    _2 = lens (\(T4 _a b _c _d) -> b) (\(T4 a _b c d) x -> T4 a x c d)
instance Field3 (T4 a b c d) (T4 a b x d) c x where
    _3 = lens (\(T4 _a _b c _d) -> c) (\(T4 a b _c d) x -> T4 a b x d)
instance Field4 (T4 a b c d) (T4 a b c x) d x where
    _4 = lens (\(T4 _a _b _c d) -> d) (\(T4 a b c _d) x -> T4 a b c x)

sfst :: T2 a b -> a
sfst (T2 a _) = a
{-# INLINE sfst #-}

ssnd :: T2 a b -> b
ssnd (T2 _ b) = b
{-# INLINE ssnd #-}

scurry :: (T2 a b -> c) -> a -> b -> c
scurry f a b = f (T2 a b)
{-# INLINE scurry #-}

suncurry :: (a -> b -> c) -> T2 a b -> c
suncurry k (T2 a b) = k a b
{-# INLINE suncurry #-}

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 k (a, b, c) = k a b c
{-# INLINE uncurry3 #-}

suncurry3 :: (a -> b -> c -> d) -> T3 a b c -> d
suncurry3 k (T3 a b c) = k a b c
{-# INLINE suncurry3 #-}

_T2 :: Iso (T2 a b) (T2 s t) (a,b) (s,t)
_T2 = iso (\(T2 a b) -> (a,b)) (uncurry T2)
{-# INLINE _T2 #-}

_T3 :: Iso (T3 a b c) (T3 s t u) (a,b,c) (s,t,u)
_T3 = iso (\(T3 a b c) -> (a,b,c)) (\(a,b,c) -> T3 a b c)
{-# INLINE _T3 #-}
