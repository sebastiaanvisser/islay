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

instance (HBinary phi (f (HFixA a f)), HBinary phi (a f (HFixA a f))) => HBinary phi (HFixA a f) where
  hput phi (HInA f) = put (0 :: Word8) >> hput phi f
  hput phi (HInF f) = put (1 :: Word8) >> hput phi f
  hget phi = do t <- getWord8
                case t of
                  0 -> HInA <$> hget phi
                  1 -> HInF <$> hget phi
                  _ -> error "HBinary phi (HFixA a f): suppress warnings"

instance HBinary phi (h (HFixA P h)) => AnnO P h phi HeapR where
  annO phi (HInA (P h)) = hretrieve phi h
  annO _   (HInF    h ) = return h

instance HBinary phi (h (HFixA P h)) => AnnO P h phi HeapW where
  annO phi (HInA (P h)) = liftLazy (hretrieve phi h)
  annO _   (HInF    h ) = return h
 
instance HBinary phi (h (HFixA P h)) => AnnI P h phi HeapW where
  annI phi = fmap (HInA . P) . hstore phi

instance HBinary phi (h (HFixA P h)) => AnnOI P h phi HeapW where

