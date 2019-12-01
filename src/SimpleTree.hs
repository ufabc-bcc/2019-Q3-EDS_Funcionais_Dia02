-- Emilio Francesquini <e.francesquini@ufabc.edu.br>
-- CC-BY-SA
-- 11/2019

{-# LANGUAGE GADTs #-}
module SimpleTree where

import Prelude hiding (elem)

data BSTree a where
  BSTEmpty :: Ord a => BSTree a
  BST :: Ord a => BSTree a -> a -> BSTree a -> BSTree a

empty :: Ord a => BSTree a
empty = BSTEmpty

singleton :: Ord a => a -> BSTree a
singleton x = BST empty x empty

null :: BSTree a -> Bool
null BSTEmpty = True
null _       = False

head :: BSTree p -> p
head (BST _ x _) = x
head _          = error "head: empty BinSearchTree"

insert :: a -> BSTree a -> BSTree a
insert x BSTEmpty = singleton x
insert x t@(BST l v r)
  | x < v     = BST (insert x l) v r
  | x > v     = BST l v (insert x r)
  | otherwise = t

elem :: a -> BSTree a -> Bool
elem _ BSTEmpty = False
elem x (BST l v r)
  | x == v    = True
  | x < v     = elem x l
  | otherwise = elem x r

-- Percurso in-ordem
toList :: BSTree a -> [a]
toList BSTEmpty = []
toList (BST l v r) =
  toList l ++  v : toList r
