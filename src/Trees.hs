-- Emilio Francesquini <e.francesquini@ufabc.edu.br>
-- CC-BY-SA
-- 11/2019

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Trees
  (
    Tree(..)
  , BinarySearchTree
  , RedBlackTree
  , TSRedBlackTree
  )
where

import Prelude hiding (elem)
import qualified SimpleTree as ST
import qualified RedBlackTree as RBT
import qualified TypeSafeRBTree as TSRB

class Ord a => Tree t a where
  empty     :: t a
  singleton :: a -> t a
  null      :: t a -> Bool
  head      :: t a -> a
  insert    :: a -> t a -> t a
  elem      :: a -> t a -> Bool
  toList    :: t a -> [a]

  fromList  :: [a] -> t a
  fromList = foldr insert empty

type BinarySearchTree = ST.BSTree
type RedBlackTree = RBT.RBTree
type TSRedBlackTree =  TSRB.RBTree

instance Ord a => Tree ST.BSTree a where
  empty     = ST.empty
  singleton = ST.singleton
  null      = ST.null
  head      = ST.head
  insert    = ST.insert
  elem      = ST.elem
  toList    = ST.toList

instance Ord a => Tree RBT.RBTree a where
  empty     = RBT.empty
  singleton = RBT.singleton
  null      = RBT.null
  head      = RBT.head
  insert    = RBT.insert
  elem      = RBT.elem
  toList    = RBT.toList

instance Ord a => Tree TSRB.RBTree a where
  empty     = TSRB.empty
  singleton = TSRB.singleton
  null      = TSRB.null
  head      = TSRB.head
  insert    = TSRB.insert
  elem      = TSRB.elem
  toList    = TSRB.toList
