{-# LANGUAGE MultiParamTypeClasses, TypeOperators, FlexibleInstances, RankNTypes #-}
module Annotation.HAnnotation where

import Control.Monad
import Control.Applicative
import Generics.HigherOrder
import Generics.Family

type In    a h phi m =  forall ix. phi ix -> h (HFixA a h) ix -> m    (HFixA a h  ix)
type Out   a h phi m =  forall ix. phi ix ->    HFixA a h  ix -> m (h (HFixA a h) ix)
type OutIn a h phi m = (forall ix. phi ix -> h (HFixA a h) ix ->    h (HFixA a h) ix)
                     -> forall ix. phi ix ->    HFixA a h  ix -> m    (HFixA a h  ix)

class (Applicative m, Monad m) => AnnO a h phi m where
  annO :: Out a h phi m

class (Applicative m, Monad m) => AnnI a h phi m where
  annI :: In a h phi m

class (AnnO a h phi m, AnnI a h phi m) => AnnOI a h phi m where
  annOI :: OutIn a h phi m
  annOI f phi = annI phi <=< return . f phi <=< annO phi

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

-- pairI
--   :: (AnnI a h phi m, PTraversable phi h)
--   => phi ix
--   ->    (HFixA a h :*: HFixA a h) ix
--   -> m ((HFixA a h :*: HFixA a h) ix)
-- pairI phi (a :*: b) = (:*:) <$> fullyIn phi a <*> fullyIn phi b

-- fullyI :: (AnnI a h phi m, PTraversable phi h, PTraversable phi g) => phi ix -> g (HFixA a h) ix -> m (g (HFixA a h) ix)
-- fullyI phi = ptraverse fullyIn phi


-- instance (Producer h, Producer g) => Producer (h :*: g) where
--   producer (f :*: g) = producer f :*: producer g

-- instance (Producer h, Producer g) => Producer (h :+: g) where
--   producer (L g) = L (producer g)
--   producer (R g) = R (producer g)

-- instance Producer b => Producer (a :-> b) where
--   producer (F f) = F (producer . f)




-- Higher order identity annotation.

instance (Applicative m, Monad m) => AnnO HId h phi m where
  annO _ (HInA (HId f)) = return f
  annO _ (HInF       f) = return f

instance (Applicative m, Monad m) => AnnI HId h phi m where
  annI _ = return . HInA . HId

instance (Applicative m, Monad m) => AnnOI HId h phi m

