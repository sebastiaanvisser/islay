{-# LANGUAGE
    DeriveFunctor
  , GeneralizedNewtypeDeriving
  , TypeOperators
  , TypeFamilies
  , RankNTypes
 #-}
module Generics.HigherOrder where

import Control.Applicative
import Data.Monoid
import Prelude

-- Functor composition.

infixl 2 :.:
data (f :.: g) a = C { unC :: f (g a) }
  deriving Functor

instance (Applicative f, Applicative g) => Applicative (f :.: g) where
  pure        = C . pure . pure
  C a <*> C b = C ((<*>) <$> a <*> b)

-- Indexed functions.

infixr 1 :->
data (:->) a b ix = F { unF :: a ix -> b ix }

(#) :: (a :-> b) ix -> a ix -> b ix
(#) (F x) y = x y

-- Indexed constant functor.

newtype K h a = K { unK :: h }
  deriving Monoid

castK :: K h a -> K h b
castK = K . unK

-- Indexed pair.

infixl 6 :+:
infixl 7 :*:

data (f :+: g) ix = L { hleft :: f ix } | R { hright :: g ix }
data (f :*: g) ix = (:*:) { hfst :: f ix, hsnd :: g ix }

type family Fst a :: *
type instance Fst (a, b) = a
type instance Fst ((a :*: b) ix) = a ix

type family Snd a :: *
type instance Snd (a, b) = b
type instance Snd ((a :*: b) ix) = b ix

-- Naturial transformation.

infixl 1 :~>
type f :~> g = forall a. f a -> g a

-- Higher order fixed point.

newtype HFix (h :: (* -> *) -> * -> *) (ix :: *) = HIn { hout :: h (HFix h) ix }

-- Higher order functor.

class HFunctor h where
  hfmap :: (a :~> b) -> h a :~> h b

instance Functor f => HFunctor ((:.:) f) where
  hfmap f = C . fmap f . unC

-- Higher order foldable and traversable.

class HFoldable h where
  hfoldMap :: Monoid m => (a :~> K m) -> h a :~> K m

foldm :: (HFoldable h, Monoid m) => (forall b. h b :~> K m) -> HFix h :~> K m
foldm f = hfoldMap (\x -> f (hout x) `mappend` foldm f x) . hout

class (HFunctor h, HFoldable h) => HTraversable h where
  htraverse :: Applicative f => (a :~> f :.: b) -> (h a :~> f :.: h b)

-- Higher order applicative.

class HApplicative h where
  hpure :: a :~> h a
  (<#>) :: (a :~> b) -> h a :~> f b

-- Higher order catamorphism.

type HAlg f g = f g :~> g

hcata :: HFunctor f => HAlg f a -> HFix f :~> a
hcata f (HIn u) = f (hfmap (hcata f) u)

-- Higher order paramorphism.

type HPara f g = f (HFix f :*: g) :~> g

hpara :: HFunctor f => HPara f a -> HFix f :~> a
hpara f (HIn u) = f (hfmap (\x -> x :*: hpara f x) u)

