{-# LANGUAGE
    TypeFamilies
  , GADTs
  , EmptyDataDecls
  , MultiParamTypeClasses
  , FlexibleContexts
  #-}
module Generics.Number where

import Generics.Proof

-- Natural numbers at the type level using Peano numbers.

data Zero
data Succ c

-- Adding more than one to a number.

type Suc2 c = Succ (Succ c)
type Suc3 c = Succ (Suc2 c)
type Suc4 c = Succ (Suc3 c)
type Suc5 c = Succ (Suc4 c)
{- ... -}

-- Continue counting.

type One   = Succ Zero
type Two   = Succ One
type Three = Succ Two
type Four  = Succ Three

-- Type family to compute the predecessor of a natural number.

type family Pred a
type instance Pred (Succ c) = c
type instance Pred  Zero    = Void

-- Proof indicating containment of type in the `Nat' datakind.

data NatPrf :: * -> * where
  ZeroP :: NatPrf Zero
  SuccP :: NatPrf n -> NatPrf (Succ n)

-- instance Show (NatPrf ix) where
--   show ZeroP     = "ZeroP"
--   show (SuccP p) = "(SuccP " ++ show p ++ ")"

-- Proof instances for natural number datakind.

instance Proof NatPrf Zero where
  proof = ZeroP
      
instance Proof NatPrf a => Proof NatPrf (Succ a) where
  proof = SuccP proof

