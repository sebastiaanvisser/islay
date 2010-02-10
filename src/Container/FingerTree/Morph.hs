{-# LANGUAGE
    GADTs
  , TypeFamilies
  , TypeOperators
  , MultiParamTypeClasses
 #-}
module Container.FingerTree.Morph where

import Prelude hiding (foldr, foldl, sum, lookup)
import Control.Applicative
import Generics.Number
import Generics.HigherOrder
import Generics.Family
import Container.FingerTree.Abstract

sumAlg :: HAlg (Tree Int) (K Int)
sumAlg (Empty         ) = K 0
sumAlg (Single a      ) = K (unK a)
sumAlg (Deep   a b c  ) = K (unK a + unK b + unK c)
sumAlg (Digit1 a      ) = K (unK a)
sumAlg (Digit2 a b    ) = K (unK a + unK b)
sumAlg (Digit3 a b c  ) = K (unK a + unK b + unK c)
sumAlg (Digit4 a b c d) = K (unK a + unK b + unK c + unK d)
sumAlg (Node2  a b    ) = K (unK a + unK b)
sumAlg (Node3  a b c  ) = K (unK a + unK b + unK c)
sumAlg (Value  a      ) = K a

sum :: FingerTree Int -> Int
sum = unK . hcata (sumAlg)

containsAlg :: Eq a => a -> HAlg (Tree a) (K Bool)
containsAlg _ (Empty         ) = K False
containsAlg _ (Single a      ) = K (unK a)
containsAlg _ (Deep   a b c  ) = K (unK a || unK b || unK c)
containsAlg _ (Digit1 a      ) = K (unK a)
containsAlg _ (Digit2 a b    ) = K (unK a || unK b)
containsAlg _ (Digit3 a b c  ) = K (unK a || unK b || unK c)
containsAlg _ (Digit4 a b c d) = K (unK a || unK b || unK c || unK d)
containsAlg _ (Node2  a b    ) = K (unK a || unK b)
containsAlg _ (Node3  a b c  ) = K (unK a || unK b || unK c)
containsAlg v (Value  a      ) = K (a == v)

contains :: Eq a => a -> FingerTree a -> Bool
contains v = unK . hcata (containsAlg v)

lookupAlg :: Eq a => a -> HAlg (Tree (a, b)) (K (Maybe b))
lookupAlg _ (Empty         ) = K Nothing
lookupAlg _ (Single a      ) = castK a
lookupAlg _ (Deep   a b c  ) = K (unK a <|> unK b <|> unK c)
lookupAlg _ (Digit1 a      ) = K (unK a)
lookupAlg _ (Digit2 a b    ) = K (unK a <|> unK b)
lookupAlg _ (Digit3 a b c  ) = K (unK a <|> unK b <|> unK c)
lookupAlg _ (Digit4 a b c d) = K (unK a <|> unK b <|> unK c <|> unK d)
lookupAlg _ (Node2  a b    ) = K (unK a <|> unK b)
lookupAlg _ (Node3  a b c  ) = K (unK a <|> unK b <|> unK c)
lookupAlg v (Value  (a, b) ) = K (if a == v then Just b else Nothing)

lookup :: Eq a => a -> FingerTree (a, b) -> Maybe b
lookup v = unK . hcata (lookupAlg v)

-- Left biased insertion.

data N   f ix = N   { unN   :: Maybe (f (Nd, Snd ix))    }
data Inc f ix = Inc { unInc :: f (Fst ix, Succ (Snd ix)) }
data Dec f ix = Dec { unDec :: f (Fst ix, Pred (Snd ix)) }

insertAlg
  :: f ~ Tree Int
  => g ~ (N (Dec (HFix f)) :-> HFix f :*: N (HFix f))
  => PPara IxPrf f g
insertAlg (SpPrf _) = worker
insertAlg (DgPrf _) = worker
insertAlg (NdPrf _) = worker
insertAlg  NdZPrf   = error "todo: prove this impossible"

worker
  :: f ~ Tree Int
  => g ~ (N (Dec (HFix f)) :-> HFix f :*: N (HFix f))
  => f (HFix f :*: g) (p, Succ c) -> g (p, Succ c)
worker u =
  case u of
    Empty          -> skeleton (\z -> single (digit1 z)                         ) Nothing
    Single b       -> skeleton (\z -> deep (hfst b) empty_ (digit1 z)           ) Nothing
    Digit1 b       -> skeleton (\z -> digit2 z (hfst b)                         ) Nothing
    Digit2 b c     -> skeleton (\z -> digit3 z (hfst b) (hfst c)                ) Nothing
    Digit3 b c d   -> skeleton (\z -> digit4 z (hfst b) (hfst c) (hfst d)       ) Nothing
    Digit4 b c d e -> skeleton (\z -> digit2 z (hfst b)                         ) (Just (node3 (hfst c) (hfst d) (hfst e)))
    Node2  b c     -> skeleton (\z -> node3  z (hfst b) (hfst c)                ) Nothing                        
    Node3  b c d   -> skeleton (\z -> node2  z (hfst b)                         ) (Just (node2 (hfst c) (hfst d)))
    Deep   b m sf  -> skeleton (\z -> deep (hfst (_b z)) (hfst (_m z)) (hfst sf)) Nothing
                        where _b z = hsnd b # N (Just (Dec z))
                              _m z = hsnd m # N (fmap Dec (unN (hsnd (_b z))))
  where skeleton result rest = F (\(N nd) -> maybe nothingToInsert (\(Dec z) -> result z :*: N rest) nd)
        nothingToInsert = HIn (hfmap hfst u) :*: N Nothing

-- Testing.

insert :: Int -> FingerTree Int -> FingerTree Int
insert inp h = hfst $ ppara insertAlg (SpPrf ZeroP) h # (N . Just . Dec) (value inp)

myTest :: Int
myTest = sum
  $ insert 8
  . insert 2
  . insert 3
  . insert 9
  . insert 3
  . insert 4
  $ empty_

