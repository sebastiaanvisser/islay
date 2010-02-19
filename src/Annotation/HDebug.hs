{-# LANGUAGE
    UndecidableInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  , KindSignatures
  , IncoherentInstances
  #-}
module Annotation.HDebug where

import Control.Applicative
import Control.Category
import Control.Monad.Trans
import Annotation.HAnnotation
import Generics.HigherOrder
import Prelude hiding ((.), id, read)

-- Higher order show type class.

class HShow h where
  hshow :: h ix -> String

instance HShow (h (HFixA a h)) => Show (h (HFixA a h) ix) where
  show = hshow

-- Higher order debug annotation.

newtype HDebug (h  :: (* -> *) -> (* -> *))
               (a  :: (* -> *))
               (ix :: *)
             = HDebug { unHDebug :: h a ix }

instance HShow (h a) => HShow (HDebug h a) where
  hshow (HDebug h) = "HDebug (" ++ hshow h ++ ")"

instance (Applicative m, MonadIO m, HShow (h (HFixA HDebug h))) => AnnO HDebug h phi m where
  annO _ (HInA (HDebug h)) = printer "query" h
  annO _ (HInF         h ) = return h

instance (Applicative m, MonadIO m, HShow (h (HFixA HDebug h))) => AnnI HDebug h phi m where
  annI _ h = HInA . HDebug <$> printer "produce" h

instance (Applicative m, MonadIO m, HShow (h (HFixA HDebug h))) => AnnOI HDebug h phi m

printer :: (MonadIO m, Show (b ix)) => String -> b ix -> m (b ix)
printer s = (\f -> liftIO (print s >> print (show f)) >> return f)

