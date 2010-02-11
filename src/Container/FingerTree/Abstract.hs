{-# LANGUAGE
    EmptyDataDecls
  , GADTs
  , TypeFamilies
  , TypeOperators
  , MultiParamTypeClasses
  , FlexibleContexts
  , FlexibleInstances
 #-}
module Container.FingerTree.Abstract where

import Prelude hiding (foldr, foldl, sum, lookup)
import Control.Applicative
import Data.List (intercalate)
import Data.Monoid
import Generics.Number
import Generics.Proof
import Generics.HigherOrder
import Generics.Family

-- Nodes in a finger tree can represent a part of the spine or a part of the
-- fingers. The top level of the fingers are the digits which can have a branch
-- level of 1-4. The lower levels of the fingers are nodes which can have a 2-3
-- branch level. At the lowest level are the values.

data Sp
data Dg
data Nd

data Tree (a :: *) (f :: * -> *) :: * -> * where
  Empty  ::                                                       Tree a f (Sp, Succ c)
  Single :: f (Dg, Succ c)                                     -> Tree a f (Sp, Succ c)
  Deep   :: f (Dg, Succ c) -> f (Sp, Suc2 c) -> f (Dg, Succ c) -> Tree a f (Sp, Succ c)
  Digit1 :: f (Nd, c)                                          -> Tree a f (Dg, Succ c)
  Digit2 :: f (Nd, c) -> f (Nd, c)                             -> Tree a f (Dg, Succ c)
  Digit3 :: f (Nd, c) -> f (Nd, c) -> f (Nd, c)                -> Tree a f (Dg, Succ c)
  Digit4 :: f (Nd, c) -> f (Nd, c) -> f (Nd, c) -> f (Nd, c)   -> Tree a f (Dg, Succ c)
  Node2  :: f (Nd, c) -> f (Nd, c)                             -> Tree a f (Nd, Succ c)
  Node3  :: f (Nd, c) -> f (Nd, c) -> f (Nd, c)                -> Tree a f (Nd, Succ c)
  Value  :: a                                                  -> Tree a f (Nd, Zero)

data IxPrf :: * -> * where
  SpPrf  :: NatPrf c -> IxPrf (Sp, Succ c)
  DgPrf  :: NatPrf c -> IxPrf (Dg, Succ c)
  NdPrf  :: NatPrf c -> IxPrf (Nd, Succ c)
  NdZPrf :: IxPrf (Nd, Zero)

instance Proof NatPrf c => Proof IxPrf (Sp, Succ c) where proof = SpPrf proof
instance Proof NatPrf c => Proof IxPrf (Dg, Succ c) where proof = DgPrf proof
instance Proof NatPrf c => Proof IxPrf (Nd, Succ c) where proof = NdPrf proof
instance                   Proof IxPrf (Nd, Zero  ) where proof = NdZPrf

-- Pretty names for common structures.

type Node       a c = HFix (Tree a) (Nd, c)
type Value      a   = Node a Zero
type Digit      a c = HFix (Tree a) (Dg, Succ c)
type Spine      a c = HFix (Tree a) (Sp, Succ c)
type FingerTree a   = HFix (Tree a) (Sp, One)

-- Bunch of smart constructors taking into the account the fixed point constructor HIn.

empty_ :: Spine a c
empty_ = HIn Empty

single :: Digit a c -> Spine a c
single a = HIn (Single a)

deep :: Digit a c -> Spine a (Succ c) -> Digit a c -> Spine a c
deep a c b = HIn (Deep a c b)

digit1 :: Node a c -> Digit a c
digit1 a = HIn (Digit1 a)

digit2 :: Node a c -> Node a c -> Digit a c
digit2 a b = HIn (Digit2 a b)

digit3 :: Node a c -> Node a c -> Node a c -> Digit a c
digit3 a b c = HIn (Digit3 a b c)

digit4 :: Node a c -> Node a c -> Node a c -> Node a c -> Digit a c
digit4 a b c d = HIn (Digit4 a b c d)

node2 :: Node a c -> Node a c -> Node a (Succ c)
node2 a b = HIn (Node2 a b)

node3 :: Node a c -> Node a c -> Node a c -> Node a (Succ c)
node3 a b c = HIn (Node3 a b c)

value :: a -> Value a
value i = HIn (Value i)

-- Show instance.

instance Show a => Show (HFix (Tree a) ix) where
  show (HIn (Empty         )) =                  "Empty"
  show (HIn (Single a      )) = intercalate " " ["(Single" , show a                        ] ++ ")"
  show (HIn (Deep   a c b  )) = intercalate " " ["(Deep"   , show a, show c, show b        ] ++ ")"
  show (HIn (Digit1 a      )) = intercalate " " ["(Digit1" , show a                        ] ++ ")"
  show (HIn (Digit2 a b    )) = intercalate " " ["(Digit2" , show a, show b                ] ++ ")"
  show (HIn (Digit3 a b c  )) = intercalate " " ["(Digit3" , show a, show b, show c        ] ++ ")"
  show (HIn (Digit4 a b c d)) = intercalate " " ["(Digit4" , show a, show b, show c, show d] ++ ")"
  show (HIn (Node2  a b    )) = intercalate " " ["(Node2"  , show a, show b                ] ++ ")"
  show (HIn (Node3  a b c  )) = intercalate " " ["(Node3"  , show a, show b, show c        ] ++ ")"
  show (HIn (Value  a      )) = intercalate " " ["(Value"  , show a                        ] ++ ")"

-- Higher order functor, foldable and traversable instances.

instance HFunctor (Tree a) where
  hfmap _ (Empty         ) = Empty
  hfmap f (Single a      ) = Single (f a)
  hfmap f (Deep   a c b  ) = Deep   (f a) (f c) (f b)
  hfmap f (Digit1 a      ) = Digit1 (f a)
  hfmap f (Digit2 a b    ) = Digit2 (f a) (f b)
  hfmap f (Digit3 a b c  ) = Digit3 (f a) (f b) (f c)
  hfmap f (Digit4 a b c d) = Digit4 (f a) (f b) (f c) (f d)
  hfmap f (Node2  a b    ) = Node2  (f a) (f b)
  hfmap f (Node3  a b c  ) = Node3  (f a) (f b) (f c)
  hfmap _ (Value  a      ) = Value  a

instance PFunctor IxPrf (Tree a) where
  pfmap _ (SpPrf _)         (Empty         ) = Empty
  pfmap f (SpPrf p)         (Single a      ) = Single (f (DgPrf p) a)
  pfmap f (SpPrf p)         (Deep   a c b  ) = Deep   (f (DgPrf p) a) (f (SpPrf (SuccP p)) c) (f (DgPrf p) b)
  pfmap f (DgPrf (SuccP p)) (Digit1 a      ) = Digit1 (f (NdPrf p) a)
  pfmap f (DgPrf (SuccP p)) (Digit2 a b    ) = Digit2 (f (NdPrf p) a) (f (NdPrf p) b)
  pfmap f (DgPrf (SuccP p)) (Digit3 a b c  ) = Digit3 (f (NdPrf p) a) (f (NdPrf p) b) (f (NdPrf p) c)
  pfmap f (DgPrf (SuccP p)) (Digit4 a b c d) = Digit4 (f (NdPrf p) a) (f (NdPrf p) b) (f (NdPrf p) c) (f (NdPrf p) d)
  pfmap f (NdPrf (SuccP p)) (Node2  a b    ) = Node2  (f (NdPrf p) a) (f (NdPrf p) b)
  pfmap f (NdPrf (SuccP p)) (Node3  a b c  ) = Node3  (f (NdPrf p) a) (f (NdPrf p) b) (f (NdPrf p) c)
  pfmap f (DgPrf ZeroP)     (Digit1 a      ) = Digit1 (f NdZPrf a)
  pfmap f (DgPrf ZeroP)     (Digit2 a b    ) = Digit2 (f NdZPrf a) (f NdZPrf b)
  pfmap f (DgPrf ZeroP)     (Digit3 a b c  ) = Digit3 (f NdZPrf a) (f NdZPrf b) (f NdZPrf c)
  pfmap f (DgPrf ZeroP)     (Digit4 a b c d) = Digit4 (f NdZPrf a) (f NdZPrf b) (f NdZPrf c) (f NdZPrf d)
  pfmap _ NdZPrf            (Value  a      ) = Value a
  pfmap _ _ _ = error "PFunctor IxPrf (Tree a): suppress warnings. "

instance HFoldable (Tree a) where
  hfoldMap _ (Empty         ) = K mempty
  hfoldMap f (Single a      ) = castK (f a)
  hfoldMap f (Deep   a c b  ) = mconcat [castK (f a), castK (f c), castK (f b)]
  hfoldMap f (Digit1 a      ) = castK (f a)
  hfoldMap f (Digit2 a b    ) = mconcat [castK (f a), castK (f b)]
  hfoldMap f (Digit3 a b c  ) = mconcat [castK (f a), castK (f b), castK (f c)]
  hfoldMap f (Digit4 a b c d) = mconcat [castK (f a), castK (f b), castK (f c), castK (f d)]
  hfoldMap f (Node2  a b    ) = mconcat [castK (f a), castK (f b)]
  hfoldMap f (Node3  a b c  ) = mconcat [castK (f a), castK (f b), castK (f c)]
  hfoldMap _ (Value  _      ) = K mempty

instance HTraversable (Tree a) where
  htraverse _ (Empty         ) = C (pure Empty)
  htraverse f (Single a      ) = C (Single <$> unC (f a))
  htraverse f (Deep   a c b  ) = C (Deep   <$> unC (f a) <*> unC (f c) <*> unC (f b))
  htraverse f (Digit1 a      ) = C (Digit1 <$> unC (f a))
  htraverse f (Digit2 a b    ) = C (Digit2 <$> unC (f a) <*> unC (f b))
  htraverse f (Digit3 a b c  ) = C (Digit3 <$> unC (f a) <*> unC (f b) <*> unC (f c))
  htraverse f (Digit4 a b c d) = C (Digit4 <$> unC (f a) <*> unC (f b) <*> unC (f c) <*> unC (f d))
  htraverse f (Node2  a b    ) = C (Node2  <$> unC (f a) <*> unC (f b))
  htraverse f (Node3  a b c  ) = C (Node3  <$> unC (f a) <*> unC (f b) <*> unC (f c))
  htraverse _ (Value  a      ) = C (pure (Value a))

instance PTraversable IxPrf (Tree a) where
  ptraverse _ (SpPrf _)         (Empty         ) = pure Empty
  ptraverse f (SpPrf p)         (Single a      ) = Single <$> f (DgPrf p) a
  ptraverse f (SpPrf p)         (Deep   a c b  ) = Deep   <$> f (DgPrf p) a <*> f (SpPrf (SuccP p)) c <*> f (DgPrf p) b
  ptraverse f (DgPrf (SuccP p)) (Digit1 a      ) = Digit1 <$> f (NdPrf p) a
  ptraverse f (DgPrf (SuccP p)) (Digit2 a b    ) = Digit2 <$> f (NdPrf p) a <*> f (NdPrf p) b
  ptraverse f (DgPrf (SuccP p)) (Digit3 a b c  ) = Digit3 <$> f (NdPrf p) a <*> f (NdPrf p) b <*> f (NdPrf p) c
  ptraverse f (DgPrf (SuccP p)) (Digit4 a b c d) = Digit4 <$> f (NdPrf p) a <*> f (NdPrf p) b <*> f (NdPrf p) c <*> f (NdPrf p) d
  ptraverse f (NdPrf (SuccP p)) (Node2  a b    ) = Node2  <$> f (NdPrf p) a <*> f (NdPrf p) b
  ptraverse f (NdPrf (SuccP p)) (Node3  a b c  ) = Node3  <$> f (NdPrf p) a <*> f (NdPrf p) b <*> f (NdPrf p) c
  ptraverse f (DgPrf ZeroP)     (Digit1 a      ) = Digit1 <$> f NdZPrf a
  ptraverse f (DgPrf ZeroP)     (Digit2 a b    ) = Digit2 <$> f NdZPrf a <*> f NdZPrf b
  ptraverse f (DgPrf ZeroP)     (Digit3 a b c  ) = Digit3 <$> f NdZPrf a <*> f NdZPrf b <*> f NdZPrf c
  ptraverse f (DgPrf ZeroP)     (Digit4 a b c d) = Digit4 <$> f NdZPrf a <*> f NdZPrf b <*> f NdZPrf c <*> f NdZPrf d
  ptraverse _ NdZPrf            (Value  a      ) = pure (Value a)
  ptraverse _ _ _ = error "PFunctor IxPrf (Tree a): suppress warnings. "

