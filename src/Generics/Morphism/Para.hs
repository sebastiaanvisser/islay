{-# OPTIONS_GHC -F -pgmF she #-}
{-# LANGUAGE GADTs, FlexibleContexts, RankNTypes, KindSignatures #-}
module Generics.Morphism.Para where

import Annotation.Annotation
import Control.Applicative
import Control.Category
import Control.Monad hiding (mapM)
import Control.Monad.Identity
import Control.Monad.Lazy
import Data.Foldable
import Data.Traversable
import Generics.Regular.Functions.Seq
import Generics.Fixpoint
import Prelude hiding ((.), id, mapM)

data AlgA (a :: (* -> *) -> * -> *) (f :: * -> *) (r :: *) where
  Psi  :: (f (FixA a f, r) -> r)  -> AlgA a f r
  Proj :: AlgA a f (r -> s, r, s) -> AlgA a f s

type Alg f r = forall a. AlgA a f r

instance Functor f => Functor (AlgA a f) where
  fmap f psi = Proj (pure f <++> psi)

instance Functor f => Applicative (AlgA a f) where
  pure    = Psi . const
  a <*> b = Proj (a <++> b)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z

instance Functor ((,,) a b) where
  fmap f (a, b, c) = (a, b, f c)

instance Foldable ((,,) a b) where
  foldMap f (_, _, c) = f c

instance Traversable ((,,) a b) where
  traverse f (a, b, c) = (| ((,,) a b) (f c) |)

(<++>) :: (Functor f, Functor (AlgA a f)) => AlgA a f (r -> s) -> AlgA a f r -> AlgA a f (r -> s, r, s)
Proj f <++> Proj g = fmap trd3 f <++> fmap trd3 g 
Psi  f <++> Proj g = Proj (pure id <++> Psi f) <++> Proj g
Proj f <++> Psi  g = Proj f <++> Proj (pure id <++> Psi g)
Psi  f <++> Psi  g = Psi (\x -> f (fmap2 fst3 x) `mk` g (fmap2 snd3 x))
  where mk x y = (x, y, x y)

_para :: (Traversable f, Lazy m, AnnO a f m) => (x -> m r) -> (r -> x) -> AlgA a f x -> FixA a f -> m r
_para z y (Proj psi) = fmap trd3 . _para (mapM z) (fmap y) psi
_para z y (Psi psi)  = z . psi <=< mapM (grp (fmap y . lazy . _para z y (Psi psi))) <=< annO
  where grp f c = fmap ((,) c) (f c)

-- Lazy paramorphism in a monadic context for annotated structures.

paraMA :: (AnnO a f m, Lazy m, Traversable f) => AlgA a f r -> FixA a f -> m r
paraMA psi = _para return id psi

-- Lazy paramorphism in a monadic context for structures without annotations.

paraM :: (Applicative m, Monad m, Lazy m, Traversable f) => AlgA Id f r -> Fix f -> m r
paraM = paraMA 

-- Lazy paramorphism for annotated structures.

paraA :: (AnnO a f Identity, Traversable f) => AlgA a f c -> FixA a f -> c
paraA psi = runIdentity . paraMA psi

-- Lazy paramorphism for structures without annotations.

para :: Traversable f => AlgA Id f c -> Fix f -> c
para psi = runIdentity . paraM psi

-- Strict paramorphism in a monadic context for annotated structures.

paraMA' :: (DeepSeq r, Traversable f, Lazy m, AnnO a f m) => AlgA a f r -> FixA a f -> m r
paraMA' psi f = (\a -> dseq a a) <$> paraMA psi f

-- Strict paramorphism in a monadic context for structures without annotations.

paraM' :: (DeepSeq r, Traversable f, Applicative m, Monad m, Lazy m) => AlgA Id f r -> Fix f -> m r
paraM' = paraMA'

-- Strict paramorphism for annotated structures.

paraA' :: (DeepSeq c, Traversable f, AnnO a f Identity) => AlgA a f c -> FixA a f -> c
paraA' psi = runIdentity . paraMA' psi

-- Strict paramorphism for structures without annotations.

para' :: (DeepSeq c, Traversable f) => AlgA Id f c -> Fix f -> c
para' psi = runIdentity . paraM' psi

type EndoA a f = AlgA a f (FixA a f)
type Endo f = forall a. EndoA a f

endoMA :: (Traversable f, AnnIO a f m, Lazy m) => EndoA a f -> FixA a f -> m (FixA a f)
endoMA psi = _para fullyIn id psi

endoM :: (Traversable f, Lazy m, Applicative m, Monad m) => EndoA Id f -> Fix f -> m (Fix f)
endoM = endoMA

endoA :: (Traversable f, AnnIO a f Identity) => EndoA a f -> FixA a f -> FixA a f
endoA psi = runIdentity . endoMA psi

endo :: Traversable f => EndoA Id f -> Fix f -> Fix f
endo psi = runIdentity . endoM psi

