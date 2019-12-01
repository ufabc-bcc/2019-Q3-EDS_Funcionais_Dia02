-- Emilio Francesquini <e.francesquini@ufabc.edu.br>
-- CC-BY-SA
-- 11/2019

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Heaps  (
    Heap(..)
  , LeftistHeap
  , BinomialHeap
  )
where

import Prelude hiding (head, tail, null)
import qualified Prelude as P

class Ord a => Heap h a where
  empty     :: h a
  singleton :: a -> h a
  null      :: h a -> Bool
  head      :: h a -> a
  tail      :: h a -> h a
  merge     :: h a -> h a -> h a

  insert    :: a -> h a -> h a
  insert a h = merge h (singleton a)

  -- Constrói um heap binário de uma lista qualquer.
  fromList  :: [a] -> h a
  fromList [] = empty
  fromList l =
    mergeList (map singleton l)
    where
      -- merge a lista até que tenha um único elemento
      mergeList [a] = a
      mergeList x = mergeList (mergePairs x)
      -- merge par a par da lista de heaps
      mergePairs (a:b:c) = merge a b : mergePairs c
      mergePairs x = x

  -- Quase um HeapSort (dependendo da impl. do heap)
  toList :: h a -> [a]
  toList h
    | null h = []
    | otherwise = head h : toList (tail h)

-- ------------
-- Leftist Heap
-- ------------
--
-- Adaptado por Emilio Francesquini <e.francesquini@ufabc.edu.br> a partir
-- do código apresentado por Chris Okasaki em Purely Functional Data Structures

data LeftistHeap a where
  LLeaf   :: Ord a => LeftistHeap a
  LNode   :: Ord a => {
    rank   :: Int,
    key    :: a,
    _left  :: LeftistHeap a,
    _right :: LeftistHeap a
  } -> LeftistHeap a

deriving instance Show a => Show (LeftistHeap a)

instance Ord a => Heap LeftistHeap a where

  -- O(1). Devolve um heap vazio.
  empty = LLeaf

  -- O(1). Devolve um heap com um único elemento.
  singleton x = LNode 0 x LLeaf LLeaf

  -- O(1). True caso heap vazio, False cc.
  null LLeaf = True
  null _     = False

  --  O(1). Devolve o elemento mínimo no heap
  head LNode {key = v} = v
  head _ = error "LeftistHeap vazio"

  --  O(lg n). Descarta a raiz e devolve o heap resultante.
  tail (LNode _ _ h1 h2) = merge h1 h2
  tail _ = error "LeftistHeap vazio"

  -- Exercício: qual a complexidade de merge?
  merge LLeaf h = h
  merge h LLeaf = h
  merge h1@(LNode _ v1 e1 d1) h2@(LNode _ v2 e2 d2)
    | v1 <= v2  = makeNode v1 e1 (merge d1 h2)
    | otherwise = makeNode v2 e2 (merge h1 d2)
    where
      rk LLeaf = -1
      rk LNode {rank = r} = r
      makeNode x a b
        | rk a >= rk b = LNode (rk b + 1) x a b
        | otherwise    = LNode (rk a + 1) x b a

-- -------------
-- Binomial Heap
-- -------------
--
-- Adaptado por Emilio Francesquini <e.francesquini@ufabc.edu.br> a partir
-- do código apresentado por Chris Okasaki em Purely Functional Data Structures

data BinomialTree a where
  BinomialTreeNode :: Ord a => {
    rnk       :: Int,
    root      :: a,
    _children :: [BinomialTree a]
    } -> BinomialTree a

deriving instance Show a => Show (BinomialTree a)


linkTree :: BinomialTree a -> BinomialTree a -> BinomialTree a
linkTree t1@(BinomialTreeNode r1 x1 c1) t2@(BinomialTreeNode _ x2 c2)
  | x1 <= x2  = BinomialTreeNode (r1 + 1) x1 (t2 : c1)
  | otherwise = BinomialTreeNode (r1 + 1) x2 (t1 : c2)

insertTree :: BinomialTree a -> [BinomialTree a] -> [BinomialTree a]
insertTree t [] = [t]
insertTree t ts@(t':ts')
  | rnk t < rnk t' = t:ts
  | otherwise      = insertTree (linkTree t t') ts'

mergeTrees :: [BinomialTree a] -> [BinomialTree a] -> [BinomialTree a]
mergeTrees ts1 [] = ts1
mergeTrees [] ts2 = ts2
mergeTrees ts1@(t1:ts1') ts2@(t2:ts2')
  | rnk t1 < rnk t2 = t1 : mergeTrees ts1' ts2
  | rnk t2 < rnk t1 = t2 : mergeTrees ts1 ts2'
  | otherwise = insertTree (linkTree t1 t2) (mergeTrees ts1' ts2')

removeMinTree :: Ord a => [BinomialTree a] -> (BinomialTree a, [BinomialTree a])
removeMinTree [] = error "empty BinomialTree"
removeMinTree [t] = (t, [])
removeMinTree (t:ts)
  | root t < root t' = (t, ts)
  | otherwise        = (t', t:ts')
  where
    (t', ts') = removeMinTree ts

newtype BinomialHeap a = BinomialHeap [BinomialTree a]

deriving instance Show a => Show (BinomialHeap a)


instance Ord a => Heap BinomialHeap a where

  empty = BinomialHeap []

  singleton x = BinomialHeap [BinomialTreeNode 0 x []]

  null (BinomialHeap ts) = P.null ts

  head (BinomialHeap ts) = root . fst $ removeMinTree ts

  tail (BinomialHeap ts) =
    BinomialHeap $ mergeTrees (reverse ts1) ts2
    where
      (BinomialTreeNode _ _ ts1, ts2) = removeMinTree ts

  merge (BinomialHeap ts1) (BinomialHeap ts2) =
    BinomialHeap $ mergeTrees ts1 ts2
