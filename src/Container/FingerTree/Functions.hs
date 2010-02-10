{-# LANGUAGE
    EmptyDataDecls
  , GADTs
  , TypeFamilies
  , TypeOperators
  , MultiParamTypeClasses
  , FlexibleContexts
  , FlexibleInstances
 #-}
module Container.FingerTree.Functions where

import Prelude hiding (foldr, foldl, sum, lookup)
import Control.Applicative
import Data.Foldable hiding (toList, sum, concat)
import Data.Monoid
import Generics.HigherOrder
import Container.FingerTree.Abstract

-- Left and right biased insertions.

infixr 5 <|

(<|) :: Node a c -> Spine a c -> Spine a c
a <| (HIn (Deep ( HIn (Digit1 b      ))   m sf)) = deep   (digit2 a b    ) m                  sf
a <| (HIn (Deep ( HIn (Digit2 b c    ))   m sf)) = deep   (digit3 a b c  ) m                  sf
a <| (HIn (Deep ( HIn (Digit3 b c d  ))   m sf)) = deep   (digit4 a b c d) m                  sf
a <| (HIn (Deep ( HIn (Digit4 b c d e))   m sf)) = deep   (digit2 a b    ) (node3 c d e <| m) sf
a <| (HIn (Single b                           )) = deep   (digit1 a)       empty_             b
a <| (HIn (Empty                              )) = single (digit1 a)

infixr 5 |>

(|>) :: Spine a c -> Node a c -> Spine a c
(HIn (Deep pr m (HIn (Digit1       b  )))) |> a = deep   pr m                  (digit2     b a)
(HIn (Deep pr m (HIn (Digit2     c b  )))) |> a = deep   pr m                  (digit3   c b a)
(HIn (Deep pr m (HIn (Digit3   d c b  )))) |> a = deep   pr m                  (digit4 d c b a)
(HIn (Deep pr m (HIn (Digit4 e d c b  )))) |> a = deep   pr (node3 e d c <| m) (digit2     b a)
(HIn (Single b                          )) |> a = deep   b  empty_             (digit1       a)
(HIn (Empty                             )) |> a = single                       (digit1       a)

(|<|) :: Foldable f => f (Value a) -> FingerTree a -> FingerTree a
(|<|) = flip (foldr (<|))

(|>|) :: Foldable f => f (Value a) -> FingerTree a -> FingerTree a
(|>|) = flip (foldl (|>))

fromList :: [Value a] -> FingerTree a
fromList = (|<| empty_)

getValue :: Tree a f ix -> [a]
getValue (Value a) = pure a
getValue _         = mempty

toList :: HFix (Tree a) c -> [a]
toList = unK . foldm (K . getValue)


