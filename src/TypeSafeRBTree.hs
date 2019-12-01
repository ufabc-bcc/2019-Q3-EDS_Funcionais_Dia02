-- Emilio Francesquini <e.francesquini@ufabc.edu.br>
-- CC-BY-SA
-- 11/2019

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module TypeSafeRBTree
  (
    RBTree,
    empty,
    singleton,
    null,
    head,
    elem,
    insert,
    toList
  )
where

import Prelude hiding (head, null, elem)

-- Regras garantidas pelo sistema de tipos
-- ✗ Regra -1: A árvore é de busca
-- ✓ Regra 0: Os nós são vermelhos ou pretos
-- ✓ Regra 1: A raiz sempre é preta
-- ✓ Regra 2: Nenhum nó vermelho tem filhos vermelhos
-- ✓ Regra 3: Os nós nulos são pretos
-- ✓ Regra 4: Altura negra dos ramos à esquerda e à direita são iguais

-- Inteiros de Peano
data Peano = Zero | Succ Peano
type One = Succ Zero

-- Node colors
data Color = R | B deriving Show

-- Apenas um atalho para os nós com cores.
type Black = Node B
type Red   = Node R

-- Obedece a regra 1
data RBTree a = forall n. T (Black n a)

-- Representa os nós da árvore.
-- GADTs usados para garantir a regra 0
-- n, representados como inteiros de Peano, verificam a garantem a
-- regra 4 da altura negra de cada ramo da árvore
data Node (c :: Color) (n :: Peano) a where
  -- Vazio (preto)
  Null  :: Ord a => Black One a -- Conforme regra 3
  -- Nós pretos podem ter filhos de qualquer cor
  Black :: Ord a => Node c1 n a -> a -> Node c2 n a -> Black (Succ n) a
  -- Nós vermelhos só podem ter filhos pretos - regra 2
  Red :: Ord a => Black n a -> a -> Black n a -> Red n a

instance Show a => Show (Node c n a) where
  show Null = "Null"
  show (Black l v r) = unwords ["(Black", show l, show v, show r] ++ ")"
  show (Red   l v r) = unwords ["(Red",   show l, show v, show r] ++ ")"

-- Devolve uma árvore vazia
empty :: Ord a => RBTree a
empty = T Null -- Obedece a regra 1

-- Devolve uma árvore contendo x
singleton :: Ord a => a -> RBTree a
singleton x = T $ Black Null x Null -- Obedece a regra 1

-- Verdadeiro se árvore vazia
null :: RBTree a -> Bool
null (T Null) = True
null _        = False

-- Devolve o primeiro elemento do ramo passado como parâmetro
head :: Ord a => RBTree a -> a
head (T Null) = error "head: empty RB Tree"
head (T (Black _ v _)) = v

-- Verdadeiro se x está em tree
elem :: Ord a => a -> RBTree a -> Bool
elem x (T node) = elemNode x node

elemNode :: a -> Node c n a -> Bool
elemNode x node =
  case node of
     Null -> False
     (Black l y r) -> elem' x l y r
     (Red l y r) -> elem' x l y r
  where
    elem' e l y r
      | e < y = elemNode e l
      | e > y = elemNode e r
      | otherwise = True


-- Ao contrário da implementação mais simples de árvores rubro-negras
-- que não se apoia nos tipos, esta implementação não permite que a
-- árvore tenha um filho vermelho abaixo de um nó vermelho (nem
-- temporariamente). Logo representamos explicitamente os casos de
-- violações da regra 2 para que a árvore seja montada diretamente na
-- sua forma correta. Veja o texto da aula para maiores informações
-- sobre os casos de violação abaixo.
data Violation (n :: Peano) a where
  Case14 -- Nós vermelhos à esq. esq.
    :: Ord a =>   -- Caso 1 Caso 4
       a          -- Red y  Red z
    -> a          -- Red x  Red y
    -> Black n a  -- a      b
    -> Black n a  -- b      c
    -> Black n a  -- c      d
    -> Violation n a
  Case23 -- Nós vermelhos à dir. dir.
    :: Ord a =>   -- Caso 2 Caso 3
       a          -- Red x  Red y
    -> a          -- Red y  Red z
    -> Black n a  -- a      b
    -> Black n a  -- b      c
    -> Black n a  -- c      d
    -> Violation n a

-- insere o elemento x na árvore t
insert :: a -> RBTree a -> RBTree a
insert x (T node) =
  case insB x node of
    (Left b) -> T b
    (Right (Red l v r)) -> T $ Black l v r

-- insere o elemento x0 na árvore de raiz negra e devolve uma nova
-- raiz que pode ser vermelha ou negra
insB :: a -> Black n a -> Either (Black n a) (Red n a)
insB x0 Null = Right $ Red Null x0 Null
insB x0 n@(Black l0 y0 r0)
  | x0 < y0 =
    case l0 of
      Null -> eitherInsBL x0 Null y0 r0
      black@Black{} -> eitherInsBL x0 black y0 r0
      red@Red{} ->
        case insR x0 red of
          (Left (Case14 y x a b c)) -> -- Caso 1
            balance a x b y c y0 r0
          (Left (Case23 x y a b c)) -> -- Caso 2
            balance a x b y c y0 r0
          (Right r) ->
            mkBlack y0 r r0
  | x0 > y0 =
    case r0 of
      Null -> eitherInsBR x0 l0 y0 Null
      black@Black{} -> eitherInsBR x0 l0 y0 black
      red@Red{} ->
        case insR x0 red of
          (Left (Case14 z y b c d)) -> -- Caso 4
            balance l0 y0 b y c z d
          (Left (Case23 y z b c d)) -> -- Caso 3
            balance l0 y0 b y c z d
          (Right r) ->
            mkBlack y0 l0 r
  | otherwise = Left n
  where
    mkBlack x a b = Left $ Black a x b
    eitherInsBR x l y r = either (mkBlack y l) (mkBlack y l) (insB x r)
    eitherInsBL x l y r = either (flip (mkBlack y) r) (flip (mkBlack y) r) (insB x l)
    balance a x b y c z d =  Right $ Red (Black a x b) y (Black c z d)

-- insere o elemento x0 na árvore de raiz vemelha e devolve uma
-- descrição sobre uma possível violação na regra 2 que possa ter
-- ocorrido. Fica sob responsabilidade do pai a correção de uma
-- possível violação.
insR :: a -> Red n a -> Either (Violation n a) (Red n a)
insR x0 n@(Red l0 y0 r0) -- l0 e r0 são pretos
  | x0 < y0 =
    case insB x0 l0 of
      (Left black)        -> mkRed black y0 r0
      (Right (Red a x b)) -> Left $ Case14 y0 x a b r0
  | x0 > y0 =
    case insB x0 r0 of
      (Left black)        -> mkRed l0 y0 black
      (Right (Red b y c)) -> Left $ Case23 y0 y l0 b c
  | otherwise = Right n
  where
    mkRed a v c = Right $ Red a v c

-- Percurso in-ordem
toList :: RBTree a -> [a]
toList (T node) = toListNode node

toListNode :: Node c n a -> [a]
toListNode node =
  case node of
    Null          -> []
    (Black l v r) -> rbToList' l v r
    (Red   l v r) -> rbToList' l v r
  where
    rbToList' a y b = toListNode a ++  y : toListNode b
