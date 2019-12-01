-- Emilio Francesquini <e.francesquini@ufabc.edu.br>
-- CC-BY-SA
-- 11/2019

module Main where

import qualified Heaps as H
import qualified Trees as T

import Data.List (sort, nub)

import Test.Tasty
import Test.Tasty.QuickCheck as QC -- Random testing

ntests :: Int
ntests = 1000

tests :: TestTree
tests = testGroup "Containers" [heapTests, treeTests]


heapTests :: TestTree
heapTests = testGroup "Heaps" [qcHeapProperties]

heapGenericProperties :: TestName ->
                     ((TestName, [Int] -> Bool) -> TestTree) ->
                     TestTree
heapGenericProperties desc f = testGroup desc $
  map f [
      ("Leftist", leftistProp)
    , ("Binomial", binomialProp)
    ]

qcHeapProperties :: TestTree
qcHeapProperties = heapGenericProperties "QC: toList . fromList == sort" tp
  where
    tp (desc, prop) = QC.testProperty desc (withMaxSuccess ntests prop)

hFromToProp :: (Ord a, H.Heap h a) => [a] -> ([a] -> h a) -> Bool
hFromToProp xs from = (H.toList . from) xs == sort xs

leftistProp :: [Int] -> Bool
leftistProp xs = hFromToProp xs from
   where
     from = H.fromList :: [Int] -> H.LeftistHeap Int

binomialProp :: [Int] -> Bool
binomialProp xs = hFromToProp xs from
   where
     from = H.fromList :: [Int] -> H.BinomialHeap Int


treeTests :: TestTree
treeTests = testGroup "Trees" [qcTreeProperties]

treeGenericProperties :: TestName ->
                     ((TestName, [Int] -> Bool) -> TestTree) ->
                     TestTree
treeGenericProperties desc f = testGroup desc $
  map f [
      ("BST", bstProp)
    , ("Red Black", redBlackProp)
    , ("Type-safe Red Black", tsRedBlackProp)
    ]

treeFromToProp :: (Ord a, T.Tree t a) => [a] -> ([a] -> t a) -> Bool
treeFromToProp xs from = (T.toList . from) xs == sort (nub xs)

qcTreeProperties :: TestTree
qcTreeProperties = treeGenericProperties "QC: toList . fromList == sort" tp
  where
    tp (desc, prop) = QC.testProperty desc (withMaxSuccess ntests prop)

bstProp :: [Int] -> Bool
bstProp xs = treeFromToProp xs from
   where
     from = T.fromList :: [Int] -> T.BinarySearchTree Int

redBlackProp :: [Int] -> Bool
redBlackProp xs = treeFromToProp xs from
   where
     from = T.fromList :: [Int] -> T.RedBlackTree Int

tsRedBlackProp :: [Int] -> Bool
tsRedBlackProp xs = treeFromToProp xs from
   where
     from = T.fromList :: [Int] -> T.TSRedBlackTree Int


-- Este módulo contém alguns testes bem básicos sobre as estruturas de
-- dados dadas em aula. Estes testes estão longe de representar uma
-- cobertura completa, mas servem como uma boa base para criação de
-- novos testes.

main :: IO ()
main = defaultMain tests
