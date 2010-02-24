{-# LANGUAGE FlexibleContexts #-}
module Container.FingerTree.PersistentMorph where

import Annotation.HAnnotation
import Annotation.HPersistent
import Container.FingerTree.Abstract
import Container.FingerTree.Morph
import Control.Applicative hiding (empty)
import Control.Monad.Lazy
import Control.Monad.Trans
import Generics.HigherOrder
import Generics.Morphism.HPara
import Generics.Number
import Heap.Heap
import Prelude hiding (sum)

type PFingerTree = FingerTree P Int

sum :: AnnO a (Tree Int) Phi m => FingerTree a Int -> m Int
sum h = unK <$> hcataA sumAlg (SpPrf ZeroP) h

-- contains :: Eq b => b -> FingerTree a b -> Bool
-- contains v = unK . hcata (containsAlg v)

-- lookup :: Eq k => k -> FingerTree a (k, v) -> Maybe v
-- lookup v = unK . hcata (lookupAlg v)

emptyFT :: AnnI a (Tree b) Phi m => m (FingerTree a b)
emptyFT = annI (SpPrf ZeroP) (houtF empty)

insert :: AnnIO a (Tree b) Phi m => b -> FingerTree a b -> m (FingerTree a b)
insert inp h =
  do f <- hparaEndoA insertAlg (SpPrf ZeroP) h
     let a = hfst (f # (N . Just . Dec) (value inp))
     fullyIn (SpPrf ZeroP) a

-- Testing.

myTest :: IO ()
myTest = run "test.txt" $
   do o  <- store nullP
      f0 <- (emptyFT :: HeapW (FingerTree P Int))
      f1 <- insert 4 f0
      f2 <- insert 3 f1
      f3 <- insert 6 f2
      unsafeReuse o (houtA f3)
--       x <- sum (f3 :: FingerTree P Int)
--       liftIO (print x)
      return ()

myTest2 :: IO ()
myTest2 = run "test.txt" $
   do t <- liftLazy $ retrieve nullPtr
      x <- sum ((HInA t) :: FingerTree P Int)
      liftIO (print x)
      return ()

