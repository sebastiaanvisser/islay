{-# LANGUAGE
    TemplateHaskell
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , EmptyDataDecls
  , FlexibleInstances
  , TypeFamilies
 #-}
module Container.Tree.Abstract where

import Data.Binary
import Data.Foldable hiding (all)
import Data.Traversable
import Generics.Regular.TH
import Generics.Fixpoint
import qualified Generics.Regular as R
import qualified Generics.Regular.Functions.Binary as G

-- | Binary tree parametrized by key type, value type and recursive positions.

data Tree k v f =
    Leaf
  | Branch
    { key     :: k
    , val     :: v
    , leftT   :: f
    , rightT  :: f
    }
  deriving (Eq, Ord, Read, Functor, Foldable, Traversable)

instance (Show k, Show v, Show f) => Show (Tree k v f) where
  show Leaf = ""
  show (Branch k v l r) =
            indent (show r) ++
    "\n" ++ show k ++ " = " ++ show v
         ++ indent (show l)

indent :: String -> String
indent = unlines . map ("\t"++) . lines 

$(deriveAll ''Tree "PFTree")
type instance R.PF (Tree k v f) = PFTree k v f

instance (Binary k, Binary v, Binary f) => Binary (Tree k v f) where
  put = G.gput
  get = G.gget

instance (Show k, Show v) => Show (FixA Id (Tree k v)) where
  show = show . unId . out

