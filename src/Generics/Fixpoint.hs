{-# LANGUAGE TypeFamilies #-}
module Generics.Fixpoint where

-- Helper functions.

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

-- Value level fixed point.

fix :: (a -> a) -> a
fix f = f (fix f)

-- Identity annotation.

newtype Id f a = Id { unId :: f a } deriving Show

-- Fixed point combinators and fixed point combinator transformers.

newtype FixA (a :: (* -> *) -> (* -> *)) (f :: (* -> *)) = In { out :: a f (FixA a f) }

type FixA1 a f = f (FixA a f)
type FixA2 a f = a f (FixA a f)

type Fix  f = FixA Id f

type Fix1 f = f (FixA Id f)
type Fix2 f = Id f (FixA Id f)

type family   Fmap (f :: * -> *) a
type instance Fmap f (a, b)       = (a, f b)
type instance Fmap f (Maybe a)    = Maybe (f a)
type instance Fmap f (Either a b) = Either a (f b)

