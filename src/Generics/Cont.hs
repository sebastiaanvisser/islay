{-# LANGUAGE ScopedTypeVariables #-}
module Generics.Cont where

import Control.Applicative
import Control.Monad
import Generics.Fixpoint
import qualified Annotation.Annotation as A

type O a f m c = FixA a f -> m c
type I a f m   = f (FixA a f) -> m (FixA a f)

type AnnO  a f m c = O a f m c                     -> f (FixA a f) -> m c
type AnnI  a f m   = I a f m                                       -> m (FixA a f)
type AnnIO a f m   = O a f m (FixA a f) -> I a f m -> f (FixA a f) -> m (FixA a f)

mkAnnO :: forall a f m c.  A.AnnO a f m => AnnO a f m c -> a f (FixA a f) -> m c
mkAnnO q = fix (q . (<=< A.annO)) <=< A.annO . InA 

mkAnnI :: A.AnnI a f m => AnnI a f m -> m (a f (FixA a f))
mkAnnI p = outa <$> p A.annI

mkAnnIO :: A.AnnIO a f m => AnnIO a f m -> a f (FixA a f) -> m (a f (FixA a f))
mkAnnIO m = liftM outa . fix (flip m A.annI . (<=< A.annO)) <=< A.annO . InA 

