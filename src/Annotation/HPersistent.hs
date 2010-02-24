{-# lANGUAGE
    TypeSynonymInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances
  , GeneralizedNewtypeDeriving
  , KindSignatures
  , IncoherentInstances
  #-}
module Annotation.HPersistent where

import Control.Applicative
import Annotation.HAnnotation
import Control.Monad.Lazy
import Data.Binary
import Data.Binary.Indexed
import Generics.HigherOrder
import Heap.Heap

-- cite: Simulating quantified class constraints where

newtype P (f  :: (* -> *) -> * -> *)
          (b  :: * -> *)
          (ix :: *)
      = P { unP :: Pointer (f b ix) }
  deriving Binary

nullP :: P f b ix
nullP = P nullPtr

instance HBinary phi (P f b) where
  hget _ = P <$> get
  hput _ = put . unP

instance HBinary phi (a f (HFixA a f)) => HBinary phi (HFixA a f) where
  hput phi (HInA f) = hput phi f
  hput _   (HInF _) = error "should not happen"
  hget phi = HInA <$> hget phi

instance HBinary phi (h (HFixA P h)) => AnnO P h phi HeapR where
  annO phi (HInA (P h)) = hretrieve phi h
  annO _   (HInF    h ) = return h

instance HBinary phi (h (HFixA P h)) => AnnO P h phi HeapW where
  annO phi (HInA (P h)) = liftLazy (hretrieve phi h)
  annO _   (HInF    h ) = return h
 
instance HBinary phi (h (HFixA P h)) => AnnI P h phi HeapW where
  annI phi = fmap (HInA . P) . hstore phi

instance HBinary phi (h (HFixA P h)) => AnnIO P h phi HeapW where

