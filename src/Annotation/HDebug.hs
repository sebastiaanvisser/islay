{-# LANGUAGE
    UndecidableInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  , KindSignatures
  #-}
module Annotation.HDebug where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad.Trans
import Annotation.HAnnotation
import Generics.HigherOrder
import Prelude hiding ((.), id, read)

-- Higher order show type class.

class HShow h where
  hshow :: h ix -> String

hprint :: HShow h => h ix -> IO ()
hprint = putStrLn . hshow

instance HShow (a h (HFixA a h)) => HShow (HFixA a h) where
  hshow = hshow . houtA

-- Higher order debug annotation.

newtype HDebug (h  :: (* -> *) -> (* -> *))
               (a  :: (* -> *))
               (ix :: *)
             = HDebug { unHDebug :: h a ix }
  deriving Show

instance HShow (h a) => HShow (HDebug h a) where
  hshow (HDebug h) = "HDebug (" ++ hshow h ++ ")"

instance (Applicative m, MonadIO m, HShow (h (HFixA HDebug h))) => HAnnQ HDebug h m where
  query = printer "query" . arr (unHDebug . houtA)

instance (Applicative m, MonadIO m, HShow (h (HFixA HDebug h))) => HAnnP HDebug h m where
  produce = printer "produce" . arr (HInA . HDebug)

printer :: (MonadIO m, HShow b) => String -> Kleisli m (b ix) (b ix)
printer s = Kleisli (\f -> liftIO (print s >> hprint f) >> return f)

