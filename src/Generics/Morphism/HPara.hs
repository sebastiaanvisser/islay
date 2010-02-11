{-# LANGUAGE KindSignatures, TypeOperators, RankNTypes, ScopedTypeVariables #-}
module Generics.Morphism.HPara where

import Control.Arrow
import Control.Monad
import Generics.HigherOrder
import Generics.Family
import Annotation.HAnnotation

type HParaA a phi f g = forall ix. phi ix -> f (HFixA a f :*: g) ix -> g ix

hparaA
  :: (HAnnQ a f m, PTraversable phi f)
  => HParaA a phi f g -> phi ix -> HFixA a f ix -> m (g ix)
hparaA psi phi =
      return . psi phi
  <=< ptraverse (\p x -> fmap ((:*:) x) $ hparaA psi p x) phi
  <=< runKleisli query

