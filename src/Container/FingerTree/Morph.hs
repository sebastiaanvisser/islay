{-# LANGUAGE
    GADTs
  , TypeFamilies
  , TypeOperators
  , DeriveFunctor
  , MultiParamTypeClasses
  , RankNTypes
 #-}
module Container.FingerTree.Morph where

import Container.FingerTree.Abstract
import Control.Applicative hiding (empty)
import Control.Monad
import Generics.HigherOrder
import Generics.Morphism.HPara
import Generics.Number
import Prelude hiding (foldr, foldl, sum, lookup)

sumAlg :: HCataA phi (Tree Int) (K Int)
sumAlg _ (Empty         ) = K 0
sumAlg _ (Single a      ) = K (unK a)
sumAlg _ (Deep   a b c  ) = K (unK a + unK b + unK c)
sumAlg _ (Digit1 a      ) = K (unK a)
sumAlg _ (Digit2 a b    ) = K (unK a + unK b)
sumAlg _ (Digit3 a b c  ) = K (unK a + unK b + unK c)
sumAlg _ (Digit4 a b c d) = K (unK a + unK b + unK c + unK d)
sumAlg _ (Node2  a b    ) = K (unK a + unK b)
sumAlg _ (Node3  a b c  ) = K (unK a + unK b + unK c)
sumAlg _ (Value  a      ) = K a

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

-- Left biased insertion.

newtype N   f ix = N   { unN   :: Maybe (f (Nd, Snd ix))    }
newtype Inc f ix = Inc { unInc :: f (Fst ix, Succ (Snd ix)) }
newtype Dec f ix = Dec { unDec :: f (Fst ix, Pred (Snd ix)) }

insertAlg
  :: f ~ Tree b
  => h ~ HFixA a f
  => g ~ (N (Dec h) :-> h :*: N h)
  => HParaA a Phi f g
insertAlg (SpPrf _) = worker
insertAlg (DgPrf _) = worker
insertAlg (NdPrf _) = worker
insertAlg  NdZPrf   = error "todo: prove this impossible"

worker
  :: f ~ Tree b
  => h ~ HFixA a f
  => g ~ (N (Dec h) :-> h :*: N h)
  => f (h :*: g) (p, Succ c) -> g (p, Succ c)
worker u =
  case u of
    Empty          -> skeleton (\z -> single (digit1 z)                         ) Nothing
    Single b       -> skeleton (\z -> deep (hfst b) empty (digit1 z)            ) Nothing
    Digit1 b       -> skeleton (\z -> digit2 z (hfst b)                         ) Nothing
    Digit2 b c     -> skeleton (\z -> digit3 z (hfst b) (hfst c)                ) Nothing
    Digit3 b c d   -> skeleton (\z -> digit4 z (hfst b) (hfst c) (hfst d)       ) Nothing
    Digit4 b c d e -> skeleton (\z -> digit2 z (hfst b)                         ) (Just (node3 (hfst c) (hfst d) (hfst e)))
    Node2  b c     -> skeleton (\z -> node3  z (hfst b) (hfst c)                ) Nothing                        
    Node3  b c d   -> skeleton (\z -> node2  z (hfst b)                         ) (Just (node2 (hfst c) (hfst d)))
    Deep   b m sf  -> skeleton (\z -> deep (hfst (b' z)) (hfst (m' z)) (hfst sf)) Nothing
                        where b' z = hsnd b # N (Just (Dec z))
                              m' z = hsnd m # N (fmap Dec (unN (hsnd (b' z))))
  where skeleton result rest = F (\(N nd) -> maybe nothingToInsert (\(Dec z) -> result z :*: N rest) nd)
        nothingToInsert = HInF (hfmap hfst u) :*: N Nothing

