{-# lANGUAGE
    TypeOperators
  , TypeSynonymInstances
  , UndecidableInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}
module Annotation.Persistent where

import Control.Applicative
import Control.Monad.Lazy
import Data.Binary
import Annotation.Annotation
import Generics.Fixpoint
import Heap.Heap

newtype P f a = P { unP :: Pointer (f a) }
  deriving Binary

instance Binary (f (FixA P f)) => AnnO P f HeapR where
  annO (InA (P f)) = retrieve f
  annO (InF    f ) = return f

instance Binary (f (FixA P f)) => AnnO P f HeapW where
  annO (InA (P f)) = liftLazy (retrieve f)
  annO (InF    f ) = return f

instance Binary (f (FixA P f)) => AnnI P f HeapW where
  annI = fmap (InA . P) . store

instance Binary (f (FixA P f)) => AnnIO P f HeapW where

instance Binary (a f (FixA a f)) => Binary (FixA a f) where
  put (InA f) = put f
  put (InF _) = error "should not happen"
  get = InA <$> get

