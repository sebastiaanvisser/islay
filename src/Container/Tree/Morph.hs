module Container.Tree.Morph where

import Generics.Fixpoint
import qualified Container.Tree.Abstract as F
import qualified Generics.Morphism.Ana as Ana ()
import qualified Generics.Morphism.Apo as Apo
import qualified Generics.Morphism.Cata as Cata ()
import qualified Generics.Morphism.Para as Para

-- Insert is WRONG! see EQ case that throws existing k v. 

insert :: Ord k => k -> v -> Apo.CoEndo (F.Tree k v)
insert k v = Apo.Phi $ \(InF s) ->
  case s of
    F.Branch m w l r ->
      case k `compare` m of
        LT -> F.Branch m w (Left  l)            (Left  r)
        EQ -> F.Branch k v (Right l)            (Left  r)
        GT -> F.Branch m w (Right l)            (Right r)
    F.Leaf -> F.Branch k v (Right (InF F.Leaf)) (Right (InF F.Leaf))

fromList :: Apo.Coalg [(k, v)] (F.Tree k v)
fromList = Apo.Phi $ \f ->
  case f of
    []        -> F.Leaf
    (k, v):xs ->
      let l = take (length xs `div` 2) xs
          r = drop (length l) xs
      in F.Branch k v (Left l) (Left r)

lookup :: Ord k => k -> Para.Alg (F.Tree k v) (Maybe v)
lookup k = Para.Psi $ \f ->
  case f of
    F.Leaf -> Nothing
    F.Branch c d (_, l) (_, r) ->
      case k `compare` c of
        LT -> l
        EQ -> Just d
        GT -> r

size :: Num n => Para.Alg (F.Tree k v) n
size = Para.Psi $ \f ->
  case f of
    F.Leaf                     -> 0
    F.Branch _ _ (_, l) (_, r) -> 1 + l + r

depth :: (Ord n, Num n) => Para.Alg (F.Tree k v) n
depth = Para.Psi $ \f ->
  case f of
    F.Leaf                     -> 0
    F.Branch _ _ (_, l) (_, r) -> 1 + max l r

