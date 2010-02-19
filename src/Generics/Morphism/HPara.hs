{-# LANGUAGE KindSignatures, TypeOperators, RankNTypes, ScopedTypeVariables #-}
module Generics.Morphism.HPara where

import Control.Monad
import Generics.HigherOrder
import Generics.Family
import Annotation.HAnnotation

type HParaA a phi f g = forall ix. phi ix -> f (HFixA a f :*: g) ix -> g ix

hparaA
  :: (AnnO a f phi m, PTraversable phi f)
  => HParaA a phi f g -> phi ix -> HFixA a f ix -> m (g ix)
hparaA psi phi =
      return . psi phi
  <=< ptraverse (\p x -> fmap ((:*:) x) $ hparaA psi p x) phi
  <=< annO phi

type HCataA phi f g = forall ix. phi ix -> f g ix -> g ix

hcataA
  :: (AnnO a f phi m, PTraversable phi f)
  => HCataA phi f g -> phi ix -> HFixA a f ix -> m (g ix)
hcataA psi phi =
      return . psi phi
  <=< ptraverse (hcataA psi) phi
  <=< annO phi




hparaEndoA
  :: (AnnO a f phi m, PTraversable phi f)
  => HParaA a phi f g -> phi ix -> HFixA a f ix -> m (g ix)
hparaEndoA psi phi =
      return . psi phi
  <=< ptraverse (\p x -> fmap ((:*:) x) $ hparaEndoA psi p x) phi
  <=< annO phi

