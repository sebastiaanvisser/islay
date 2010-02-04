{-# lANGUAGE
    TypeOperators
  , TypeSynonymInstances
  , UndecidableInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}
module Annotation.Persistent where

import Prelude hiding ((.))
import Control.Applicative
import Control.Category
import Control.Arrow
import Control.Monad.Lazy
import Data.Binary
import Annotation.Annotation
import Generics.Types
import Heap.Heap

newtype P f a = P { unP :: Pointer (f a) }
  deriving Binary

instance Binary (f (FixA P f)) => AnnQ P f HeapR where
  query = Kleisli (retrieve . unP . out)

instance Binary (f (FixA P f)) => AnnQ P f HeapW where
  query = Kleisli (liftLazy . retrieve . unP . out)

instance Binary (f (FixA P f)) => AnnP P f HeapW where
  produce = Kleisli (fmap (In . P) . store)

instance Binary (f (FixA P f)) => AnnM P f HeapW where

instance Binary (a f (FixA a f)) => Binary (FixA a f) where
  put = put . out
  get = In <$> get

