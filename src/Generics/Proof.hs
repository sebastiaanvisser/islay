{-# LANGUAGE EmptyDataDecls, GADTs, MultiParamTypeClasses #-}
module Generics.Proof where

-- Type without any inhabitants.

data Void

-- Data type encoding type equality.

data EQ a b where
  Refl :: EQ a a

cast :: EQ a b -> a -> b
cast Refl a = a

-- Proof that some index `ix' is an element of some family `phi'.

class Proof phi ix where
  proof :: phi ix

