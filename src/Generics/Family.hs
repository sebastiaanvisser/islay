{-# LANGUAGE
    MultiParamTypeClasses
  , RankNTypes
  , TypeOperators
  , TypeFamilies
  , FlexibleInstances
  #-}
module Generics.Family where

import Control.Applicative
import Generics.HigherOrder

class PFunctor phi h where
  pfmap :: (forall jx. phi jx -> a jx -> b jx) -> forall ix. phi ix -> h a ix -> h b ix

instance PFunctor phi ((:->) a) where
  pfmap f phi (F g) = F (f phi . g)

class PFunctor phi h => PTraversable phi h where
  ptraverse :: Applicative f => (forall ix. phi ix -> a ix -> f (b ix)) -> forall ix. phi ix -> h a ix -> f (h b ix)

