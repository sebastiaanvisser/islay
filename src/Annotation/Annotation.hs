{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleInstances
  #-}
module Annotation.Annotation where

import Control.Applicative
import Control.Monad
import Data.Traversable
import Generics.Fixpoint

type In    a f m =  (f (FixA a f)) -> m (  (FixA a f))
type Out   a f m =  (  (FixA a f)) -> m (f (FixA a f))
type InOut a f m = ((f (FixA a f)) -> m (f (FixA a f)))
                 -> (  (FixA a f)) -> m (  (FixA a f))

class (Applicative m, Monad m) => AnnO a f m where
  annO :: Out a f m

class (Applicative m, Monad m) => AnnI a f m where
  annI :: In a f m

class (AnnO a f m, AnnI a f m) => AnnIO a f m where
  annIO :: InOut a f m
  annIO f = annI <=< f <=< annO

-- Remove all annotations from a recursive structure. This function assumes
-- that an unannotated node can never have any annotated descendants.

fullyOut :: (Traversable f, AnnO a f m) => FixA a f -> m (FixA a f)
fullyOut (InA f) = annO (InA f) >>= fmap InF . traverse fullyOut
fullyOut a       = return a

-- Fully annotate a recursive structure. This function assumes that an
-- annotated node can never have any descendants without annotation.

fullyIn :: (Traversable f, AnnI a f m) => FixA a f -> m (FixA a f)
fullyIn (InF f) = traverse fullyIn f >>= annI 
fullyIn a       = return a

-- The instance for the identity annotation just unpacks the constructor.

instance (Applicative m, Monad m) => AnnO Id f m where
  annO (InA (Id f)) = return f
  annO (InF     f ) = return f

instance (Applicative m, Monad m) => AnnI Id f m where
  annI = return . InA . Id

instance (Applicative m, Monad m) => AnnIO Id f m

