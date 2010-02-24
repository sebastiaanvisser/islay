{-# LANGUAGE
    TypeOperators
  , GADTs
  , KindSignatures
  , FlexibleContexts
  , RankNTypes
  , ScopedTypeVariables
  #-}
module Generics.Morphism.Apo where

import Annotation.Annotation
import Control.Applicative
import Control.Monad hiding (mapM)
import Control.Monad.Identity
import Data.Traversable
import Generics.Fixpoint
import Prelude hiding (mapM)

data CoalgA (a :: (* -> *) -> * -> *) (f :: * -> *) (s :: *) where
  Phi :: (s -> f (Either s (FixA a f))) -> CoalgA a f s

type Coalg s f = forall a. CoalgA a f s

apoMA :: (Traversable f, AnnIO a f m) => CoalgA a f s -> s -> m (FixA a f)
apoMA (Phi phi) = annI <=< mapM (apoMA (Phi phi) `either` return) . phi

apoM :: (Traversable f, AnnIO Id f m) => CoalgA Id f s -> s -> m (Fix f)
apoM = apoMA

apoA :: (Traversable f, AnnIO a f Identity) => CoalgA a f s -> s -> FixA a f
apoA phi = runIdentity . apoMA phi

apo :: Traversable f => CoalgA Id f s -> s -> Fix f
apo phi = runIdentity . apoM phi

type CoEndoA a f = CoalgA a f (FixA a f)
type CoEndo    f = forall a. CoEndoA a f

endoMA :: (Traversable f, AnnIO a f m) => CoEndoA a f -> FixA a f -> m (FixA a f)
endoMA (Phi phi) = annIO (mapM (either (endoMA (Phi phi)) fullyIn) . phi . InF)

endoM :: (Traversable f, Applicative m, Monad m) => CoEndoA Id f -> Fix f -> m (Fix f)
endoM = endoMA

endoA :: (Traversable f, AnnIO a f Identity) => CoEndoA a f -> FixA a f -> FixA a f
endoA phi = runIdentity . endoMA phi

endo :: Traversable f => CoEndoA Id f -> Fix f -> Fix f
endo phi = runIdentity . endoM phi

