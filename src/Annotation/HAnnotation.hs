{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleInstances
  , RankNTypes
  #-}
module Annotation.HAnnotation where

import Control.Applicative
import Control.Monad
import Generics.Family
import Generics.HigherOrder

type In    a h phi m =  forall ix. phi ix -> h (HFixA a h) ix -> m    (HFixA a h  ix)
type Out   a h phi m =  forall ix. phi ix ->    HFixA a h  ix -> m (h (HFixA a h) ix)
type InOut a h phi m = (forall ix. phi ix -> h (HFixA a h) ix ->    h (HFixA a h) ix)
                     -> forall ix. phi ix ->    HFixA a h  ix -> m    (HFixA a h  ix)

class (Applicative m, Monad m) => AnnO a h phi m where
  annO :: Out a h phi m

class (Applicative m, Monad m) => AnnI a h phi m where
  annI :: In a h phi m

class (AnnO a h phi m, AnnI a h phi m) => AnnIO a h phi m where
  annIO :: InOut a h phi m
  annIO f phi = annI phi . f phi <=< annO phi

-- Remove all annotations from a recursive structure. This function assumes
-- that an unannotated node can never have any annotated descendants.

fullyOut :: (AnnO a h phi m, PTraversable phi h) => phi ix -> HFixA a h ix -> m (HFixA a h ix)
fullyOut phi (HInA f) = annO phi (HInA f) >>= fmap HInF . ptraverse fullyOut phi
fullyOut _   a        = return a

-- Fully annotate a recursive structure. This function assumes that an
-- annotated node can never have any descendants without annotation.

fullyIn :: (AnnI a h phi m, PTraversable phi h) => phi ix -> HFixA a h ix -> m (HFixA a h ix)
fullyIn phi (HInF f) = ptraverse fullyIn phi f >>= annI phi
fullyIn _   a        = return a

-- The instance for the identity annotation just unpacks the constructor.

instance (Applicative m, Monad m) => AnnO HId h phi m where
  annO _ (HInA (HId f)) = return f
  annO _ (HInF      f ) = return f

instance (Applicative m, Monad m) => AnnI HId h phi m where
  annI _ = return . HInA . HId

instance (Applicative m, Monad m) => AnnIO HId h phi m

