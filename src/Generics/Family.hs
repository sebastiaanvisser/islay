{-# LANGUAGE MultiParamTypeClasses, RankNTypes, TypeOperators, TypeFamilies #-}
module Generics.Family where

import Control.Applicative
import Generics.HigherOrder

class PFunctor phi h where
  pfmap :: (forall jx. phi jx -> a jx -> b jx) -> forall ix. phi ix -> h a ix -> h b ix

type PPara phi f g = forall ix. phi ix -> f (HFix f :*: g) ix -> g ix

ppara :: PFunctor phi f => PPara phi f g -> phi ix -> HFix f ix -> g ix
ppara f phi (HIn u) = f phi (pfmap (\p x -> x :*: ppara f p x) phi u)

class PFunctor phi h => PTraversable phi h where
  ptraverse :: Applicative f => (forall ix. phi ix -> a ix -> f (b ix)) -> (forall ix. phi ix -> h a ix -> f (h b ix))

