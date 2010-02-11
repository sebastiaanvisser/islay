{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, RankNTypes #-}
module Annotation.HAnnotation where

import Control.Applicative
import Control.Arrow
import Control.Category
import Generics.HigherOrder
import Prelude hiding ((.), id)

type Produce a h m = forall ix. Kleisli m (h (HFixA a h) ix)  (  (HFixA a h  ix))
type Query   a h m = forall ix. Kleisli m    (HFixA a h  ix)  (h (HFixA a h) ix)
type Modify  a h m = forall ix. Kleisli m (h (HFixA a h) ix)  (h (HFixA a h) ix)
                             -> Kleisli m (  (HFixA a h  ix)) (  (HFixA a h  ix))

class (Applicative m, Monad m) => HAnnQ a h m where
  query :: Query a h m

class (Applicative m, Monad m) => HAnnP a h m where
  produce :: Produce a h m

class (HAnnQ a h m, HAnnP a h m) => HAnnM a h m where
  modify :: Modify a h m
  modify f = produce . f . query

runQuery :: HAnnQ a h m => HFixA a h ix -> m (h (HFixA a h) ix)
runQuery = runKleisli query

runProduce :: HAnnP a h m => h (HFixA a h) ix -> m (HFixA a h ix)
runProduce = runKleisli produce

instance (Applicative m, Monad m) => HAnnQ HId h m where
  query = Kleisli (return . unHId . houtA)

instance (Applicative m, Monad m) => HAnnP HId h m where
  produce = Kleisli (return . HInA . HId)

instance (Applicative m, Monad m) => HAnnM HId h m

