-- Emilio Francesquini <e.francesquini@ufabc.edu.br>
-- CC-BY-SA
-- 11/2019

{-# LANGUAGE GADTs #-}

module RedBlackTree where

import Prelude hiding (elem)

data RBColor = R | B
data RBTree a where
  RBEmpty :: Ord a => RBTree a
  RBT     :: Ord a => RBColor -> RBTree a -> a -> RBTree a -> RBTree a

empty :: Ord a => RBTree a
empty = RBEmpty

singleton :: Ord a => a -> RBTree a
singleton x = RBT B empty x empty

null :: RBTree a -> Bool
null RBEmpty = True
null _       = False

head :: RBTree p -> p
head (RBT _ _ x _) = x
head _          = error "head: empty Red Black Tree"

buildRed :: Ord a => RBTree a -> a -> RBTree a -> a -> RBTree a -> a -> RBTree a -> RBTree a
buildRed a x b y c z d = RBT R (RBT B a x b) y (RBT B c z d)

balance :: Ord a => RBColor -> RBTree a -> a -> RBTree a -> RBTree a
balance B (RBT R (RBT R a x b) y c) z d = buildRed a x b y c z d -- Caso 1
balance B (RBT R a x (RBT R b y c)) z d = buildRed a x b y c z d -- Caso 2
balance B a x (RBT R b y (RBT R c z d)) = buildRed a x b y c z d -- Caso 3
balance B a x (RBT R (RBT R b y c) z d) = buildRed a x b y c z d -- Caso 4
balance color a x b = RBT color a x b

insert :: Ord a => a -> RBTree a -> RBTree a
insert x t = makeBlack $ ins t
  where
    ins RBEmpty = RBT R empty x empty
    ins t2@(RBT color l y r)
      | x < y = balance color (ins l) y r
      | x > y = balance color l y (ins r)
      | otherwise = t2

    makeBlack ~(RBT _ a y b) = RBT B a y b

elem :: a -> RBTree a -> Bool
elem _ RBEmpty = False
elem x (RBT _ l y r)
  | x < y = elem x l
  | x > y = elem x r
  | otherwise = True

-- Percurso in-ordem
toList :: RBTree a -> [a]
toList RBEmpty = []
toList (RBT _ l v r) =
  toList l ++  v : toList r
