{-# LANGUAGE TypeFamilies #-}
module Generics.Fixpoint where

-- Deep fmap.

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

-- Value level fixed point.

fix :: (a -> a) -> a
fix f = f (fix f)

-- Identity annotation.

newtype Id f a = Id { unId :: f a } deriving Show

-- Annotated fixed point combinator. The optional annotation is stored in the
-- InA constructor and can be skipped by using the Inf constructor.

data FixA (a :: (* -> *) -> (* -> *))
          (f :: (* -> *))
  = InA { outa :: a f (FixA a f) }
  | InF { outf ::   f (FixA a f) }

-- A regular fixed point can be obtained by using the identity annotation.

type Fix  f = FixA Id f

-- Type level functor.

type family   Fmap (f :: * -> *) a
type instance Fmap f (a, b)       = (a, f b)
type instance Fmap f (Maybe a)    = Maybe (f a)
type instance Fmap f (Either a b) = Either a (f b)

