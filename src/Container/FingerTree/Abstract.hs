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

import Annotation.HDebug
import Control.Applicative
import Data.List (intercalate)
import Data.Monoid
import Data.Binary
import Data.Binary.Indexed
import Generics.Family
import Generics.HigherOrder
import Generics.Number
import Generics.Proof
import Prelude hiding (foldr, foldl, sum, lookup)

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

data Phi :: * -> * where
  SpPrf  :: NatPrf c -> Phi (Sp, Succ c)
  DgPrf  :: NatPrf c -> Phi (Dg, Succ c)
  NdPrf  :: NatPrf c -> Phi (Nd, Succ c)
  NdZPrf :: Phi (Nd, Zero)

instance Proof NatPrf c => Proof Phi (Sp, Succ c) where proof = SpPrf proof
instance Proof NatPrf c => Proof Phi (Dg, Succ c) where proof = DgPrf proof
instance Proof NatPrf c => Proof Phi (Nd, Succ c) where proof = NdPrf proof
instance                   Proof Phi (Nd, Zero  ) where proof = NdZPrf

-- Pretty names for common structures.

type Node       a b c = HFixA a (Tree b) (Nd, c)
type Value      a b   = Node a b Zero
type Digit      a b c = HFixA a (Tree b) (Dg, Succ c)
type Spine      a b c = HFixA a (Tree b) (Sp, Succ c)
type FingerTree a b   = HFixA a (Tree b) (Sp, One)

-- Bunch of smart constructors taking into the account the fixed point constructor HInF.

empty :: Spine a b c
empty = HInF Empty

single :: Digit a b c -> Spine a b c
single a = HInF (Single a)

deep :: Digit a b c -> Spine a b (Succ c) -> Digit a b c -> Spine a b c
deep a c b = HInF (Deep a c b)

digit1 :: Node a b c -> Digit a b c
digit1 a = HInF (Digit1 a)

digit2 :: Node a b c -> Node a b c -> Digit a b c
digit2 a b = HInF (Digit2 a b)

digit3 :: Node a b c -> Node a b c -> Node a b c -> Digit a b c
digit3 a b c = HInF (Digit3 a b c)

digit4 :: Node a b c -> Node a b c -> Node a b c -> Node a b c -> Digit a b c
digit4 a b c d = HInF (Digit4 a b c d)

node2 :: Node a b c -> Node a b c -> Node a b (Succ c)
node2 a b = HInF (Node2 a b)

node3 :: Node a b c -> Node a b c -> Node a b c -> Node a b (Succ c)
node3 a b c = HInF (Node3 a b c)

value :: b -> Value a b
value i = HInF (Value i)

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

instance HShow (Tree a ix) where
  hshow (Empty  {}) = "Empty ..."
  hshow (Single {}) = "Single ..."
  hshow (Deep   {}) = "Deep ..."
  hshow (Digit1 {}) = "Digit1 ..."
  hshow (Digit2 {}) = "Digit2 ..."
  hshow (Digit3 {}) = "Digit3 ..."
  hshow (Digit4 {}) = "Digit4 ..."
  hshow (Node2  {}) = "Node2 ..."
  hshow (Node3  {}) = "Node3 ..."
  hshow (Value  {}) = "Value ..."

instance (Binary a, HBinary Phi f) => HBinary Phi (Tree a f) where
  hget (SpPrf p) =
    do t <- getWord8
       case t of
         0 -> return Empty
         1 -> Single <$> hget (DgPrf p)
         2 -> Deep   <$> hget (DgPrf p) <*> hget (SpPrf (SuccP p)) <*> hget (DgPrf p)
         _ -> error "HBinary Phi (Tree a f)"
  hget (DgPrf (SuccP p)) =
    do t <- getWord8
       case t of
         0 -> Digit1 <$> hget (NdPrf p)
         1 -> Digit2 <$> hget (NdPrf p) <*> hget (NdPrf p)
         2 -> Digit3 <$> hget (NdPrf p) <*> hget (NdPrf p) <*> hget (NdPrf p)
         3 -> Digit4 <$> hget (NdPrf p) <*> hget (NdPrf p) <*> hget (NdPrf p) <*> hget (NdPrf p)
         _ -> error "HBinary Phi (Tree a f)"
  hget (NdPrf (SuccP p)) =
    do t <- getWord8
       case t of
         0 -> Node2 <$> hget (NdPrf p) <*> hget (NdPrf p)
         1 -> Node3 <$> hget (NdPrf p) <*> hget (NdPrf p) <*> hget (NdPrf p)
         _ -> error "HBinary Phi (Tree a f)"
  hget (DgPrf ZeroP) =
    do t <- getWord8
       case t of
         0 -> Digit1 <$> hget NdZPrf
         1 -> Digit2 <$> hget NdZPrf <*> hget NdZPrf
         2 -> Digit3 <$> hget NdZPrf <*> hget NdZPrf <*> hget NdZPrf
         3 -> Digit4 <$> hget NdZPrf <*> hget NdZPrf <*> hget NdZPrf <*> hget NdZPrf
         _ -> error "HBinary Phi (Tree a f)"
  hget NdZPrf = Value <$> get
  hget _ = error "HBinary Phi (Tree a f): suppress warnings"

  hput (SpPrf _)         (Empty         ) = put (0 :: Word8)
  hput (SpPrf p)         (Single a      ) = put (1 :: Word8) >> hput (DgPrf p) a
  hput (SpPrf p)         (Deep   a c b  ) = put (2 :: Word8) >> hput (DgPrf p) a >> hput (SpPrf (SuccP p)) c >> hput (DgPrf p) b
  hput (DgPrf (SuccP p)) (Digit1 a      ) = put (0 :: Word8) >> hput (NdPrf p) a
  hput (DgPrf (SuccP p)) (Digit2 a b    ) = put (1 :: Word8) >> hput (NdPrf p) a >> hput (NdPrf p) b
  hput (DgPrf (SuccP p)) (Digit3 a b c  ) = put (2 :: Word8) >> hput (NdPrf p) a >> hput (NdPrf p) b >> hput (NdPrf p) c
  hput (DgPrf (SuccP p)) (Digit4 a b c d) = put (3 :: Word8) >> hput (NdPrf p) a >> hput (NdPrf p) b >> hput (NdPrf p) c >> hput (NdPrf p) d
  hput (NdPrf (SuccP p)) (Node2  a b    ) = put (0 :: Word8) >> hput (NdPrf p) a >> hput (NdPrf p) b
  hput (NdPrf (SuccP p)) (Node3  a b c  ) = put (1 :: Word8) >> hput (NdPrf p) a >> hput (NdPrf p) b >> hput (NdPrf p) c
  hput (DgPrf ZeroP)     (Digit1 a      ) = put (0 :: Word8) >> hput NdZPrf a
  hput (DgPrf ZeroP)     (Digit2 a b    ) = put (1 :: Word8) >> hput NdZPrf a >> hput NdZPrf b
  hput (DgPrf ZeroP)     (Digit3 a b c  ) = put (2 :: Word8) >> hput NdZPrf a >> hput NdZPrf b >> hput NdZPrf c
  hput (DgPrf ZeroP)     (Digit4 a b c d) = put (3 :: Word8) >> hput NdZPrf a >> hput NdZPrf b >> hput NdZPrf c >> hput NdZPrf d
  hput NdZPrf            (Value  a      ) = put a
  hput _ _ = error "PFunctor Phi (Tree a): suppress warnings. "

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

instance PFunctor Phi (Tree a) where
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
  pfmap _ _ _ = error "PFunctor Phi (Tree a): suppress warnings. "

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
  htraverse _ (Empty         ) = pure Empty
  htraverse f (Single a      ) = Single <$> f a
  htraverse f (Deep   a c b  ) = Deep   <$> f a <*> f c <*> f b
  htraverse f (Digit1 a      ) = Digit1 <$> f a
  htraverse f (Digit2 a b    ) = Digit2 <$> f a <*> f b
  htraverse f (Digit3 a b c  ) = Digit3 <$> f a <*> f b <*> f c
  htraverse f (Digit4 a b c d) = Digit4 <$> f a <*> f b <*> f c <*> f d
  htraverse f (Node2  a b    ) = Node2  <$> f a <*> f b
  htraverse f (Node3  a b c  ) = Node3  <$> f a <*> f b <*> f c
  htraverse _ (Value  a      ) = pure (Value a)

instance PTraversable Phi (Tree a) where
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
  ptraverse _ _ _ = error "PFunctor Phi (Tree a): suppress warnings. "

