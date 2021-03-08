{-
  PP, laboratorul 8: tipuri de date utilizator
-}

import Data.List
import Data.Maybe
import TestPP

{-
  1. (2p) Vectori
  Se dă tipul de date Vector, reprezentând vectori din spațiul R^3.

  Implementați următoarele operații cu vectori:
  - norma unui vector
  - normalizarea unui vector
  - produsul scalar (dot product) dintre doi vectori
  - produsul vectorial (cross product) dintre doi vectori
  - adunarea a doi vectori
  - scăderea a doi vectori
  - verificarea ortogonalității unor vectori

  Explicații

  Fie a și b doi vectori din R^3 considerați de forma:
  a = a1 * i + a2 * j + a3 * k
  b = b1 * i + b2 * j + b3 * k
  Norma vectorului a este egală cu:
  |a| = sqrt(a1^2 + a2^2 + a3^2)
  Normalizarea vectorului a este egală cu:
  a' = a / |a| = (a1 / |a|) * i + (a2 / |a|) * j + (a3 / |a|) * k
  Produsul vectorial al celor doi vectori o să fie egal cu:
  a x b = (a2 * b3 - a3 * b2) * i + (a3 * b1 - a1 * b3) * j + (a1 * b2 - a2 * b1) * k
  Produsul scalar al celor doi vectori o să fie egal cu:
  a • b = a1 * b1 + a2 * b2 + a3 * b3
  Produsul scalar a doi vectori u și v este 0 dacă și numai dacă u și v sunt ortogonali.

  Pentru mai multe detalii, consultați:
  https://gerardnico.com/linear_algebra/vector_vector
-}
data Vector = V
  { vx :: Double
  , vy :: Double
  , vz :: Double
  } deriving (Show, Eq)

lengthV :: Vector -> Double
lengthV v = sqrt (x*x + y*y + z*z)
		where
			x = vx v
			y = vy v
			z = vz v

normalizeV :: Vector -> Vector
normalizeV v = V (x / n) (y / n) (z / n)
		where
			x = vx v
			y = vy v
			z = vz v
			n = lengthV v
			
dotV :: Vector -> Vector -> Double
dotV v1 v2 = x1 * x2 + y1 * y2 + z1 * z2
		where
			x1 = vx v1
			y1 = vy v1
			z1 = vz v1
			x2 = vx v2
			y2 = vy v2
			z2 = vz v2

crossV :: Vector -> Vector -> Vector
crossV v1 v2 = V (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x1)
		where
			x1 = vx v1
			y1 = vy v1
			z1 = vz v1
			x2 = vx v2
			y2 = vy v2
			z2 = vz v2

addV :: Vector -> Vector -> Vector
addV v1 v2 = V (x1 + x2) (y1 + y2) (z1 + z2)
		where
			x1 = vx v1
			y1 = vy v1
			z1 = vz v1
			x2 = vx v2
			y2 = vy v2
			z2 = vz v2

subV :: Vector -> Vector -> Vector
subV v1 v2 = V (x1 - x2) (y1 - y2) (z1 - z2)
		where
			x1 = vx v1
			y1 = vy v1
			z1 = vz v1
			x2 = vx v2
			y2 = vy v2
			z2 = vz v2

orthogonalV :: [Vector] -> Bool
orthogonalV vectors = and [dotV x y == 0| x <- vectors, y <- vectors, x /= y]

check1 :: TestData
check1 = do
  let v1 = V 1 (-1) 0
      v2 = V 1 1 0
      v3 = V 0 0 0
  tests 1 2 $
          [ testVal "lengthV" 1 (lengthV v1) (sqrt 2)
          , testVal "normalizeV" 1 (normalizeV (V 0 (-1) 0)) (V 0 (-1) 0)
          , testVal "dotV" 1 (dotV v1 v2) 0.0
          , testVal "crossV" 1 (crossV v1 v2) (V 0 0 2)
          , testVal "addV" 1 (addV v1 v2) (V 2 0 0)
          , testVal "subV" 1 (subV v2 v3) v2
          , testCond "orthogonalV" 2 (orthogonalV [v1, v2, v3])]

{-
 2. (4p) Liste imbricate
  Definiți un tip de date SList a care să aibă funcționalități
  asemănătoare listelor din limbajele Lisp (e.g. Scheme, Racket, Clojure),
  permițând componente la diferite niveluri de imbricare.
  Ex: Lista din Racket '(1 (3 4) (2)) să poată fi definită în Haskell
  folosind SList.
  Adițional, definiți:
  - emptySList, lista vidă
  - consElem, adaugă un element în capul unei liste
    Ex: consElem 1 '((3 4) (2)) == '(1 (3 4) (2))
  - consList, adaugă o listă (imbricată) în capul unei liste
    Ex: consList '(2 3) '(1 2) == '((2 3) 1 2)
  - headSList, ia primul element dintr-un SList
  - tailSList, ia restul SList-ului
  - deepEqual, o funcție ce verifică egalitatea a două SList
  - flatten, întoarce lista cu elementele din SList (pe același nivel)
  Notare:
  (2p) constructorii (emptySList, consElem și consList) și deepEqual
  (1p) headSList și tailSList
  (1p) flatten
-}
data SList a = Empty | Single a | List {elemente :: [SList a]} deriving Show

emptySList :: SList a
emptySList = Empty 

consElem :: a -> SList a -> SList a
consElem x Empty = List [Single x]
consElem x (Single y) = List [Single x, Single y] 
consElem x lista = List $ [Single x] ++ (elemente lista)

consList :: SList a -> SList a -> SList a
consList lista1 lista2 = List [lista1, lista2]

headSList :: SList a -> SList a
headSList (List []) = Empty 
headSList Empty = Empty
headSList lista = head $ elemente lista

tailSList :: SList a -> SList a
tailSList (List []) = Empty
tailSList (Single x) = Empty
tailSList Empty = Empty
tailSList lista = List $ tail $ elemente lista

deepEqual :: Eq a => SList a -> SList a -> Bool
deepEqual Empty Empty = True
deepEqual _ Empty = False
deepEqual Empty _ = False
deepEqual (Single x) (List y) = False
deepEqual (List x) (Single y) = False
deepEqual (Single x) (Single y) = if x == y then True else False
deepEqual lista1 lista2 = and (auxf0 lista1 lista2)

auxf0 Empty Empty = [True]
auxf0 Empty _ = [False]
auxf0 _ Empty = [False]
auxf0 lista1 lista2 = (auxf1 (headSList lista1) (headSList lista2)) ++ (auxf0 (tailSList lista1) (tailSList lista2))

auxf1 Empty Empty = [True]
auxf1 _ Empty = [False]
auxf1 Empty _ = [False]
auxf1 (Single x) (List y) = [False]
auxf1 (List x) (Single y) = [False]
auxf1 (Single x) (Single y) = if x == y then [True] else [False]
auxf1 lista1 lista2 = (auxf1 (headSList lista1) (headSList lista2)) ++ (auxf1 (tailSList lista1) (tailSList lista2))

flatten :: SList a -> [a]
flatten Empty = []
flatten lista = (auxf2 (headSList lista)) ++ (flatten (tailSList lista))

auxf2 Empty = []
auxf2 (Single x) = [x]
auxf2 lista = (auxf2 $ headSList lista) ++ (auxf2 $ tailSList lista)

check2 :: TestData
check2 = do
  let l1 = consElem 1 $ emptySList
      l2 = consElem 2 $ consList (consElem 1 $ consElem 1 emptySList) $
           consElem 3 emptySList
      l3 = consList (consElem 1 $ consElem 1 emptySList) $ consElem 3 $
           emptySList
  tests 2 4 $
          [ testCond "simple lists" 1 $ deepEqual l1 l1 && not (deepEqual l1 l2)
          , testCond "less simple lists" 1 $ deepEqual (consElem 2 $ l3) l2
          , testCond "head, tail" 1 $ deepEqual (headSList $ tailSList l2)
                                   (consElem 1 $ consElem 1 emptySList)
          , testCond "flatten" 1 $ flatten l2 == [2,1,1,3]]

{-
  3. (4p) Arbori binari de căutare
  Definiți un tip de date BST a pentru a implementa un arbore binar de
  căutare. De asemenea, definiți funcții pentru a insera o valoare într-un 
  arbore binar de căutare, căutarea unui element într-un arbore binar de 
  căutare dat, o funcție care întoarce lista elementelor din parcurgerea
  în inordine a arborelui și o funcție care întoarce cel mai mic subarbore
  care conține două noduri ale căror chei sunt date.
-}
data BST a = BSTNil | BSTNode {element :: a} | BST {root :: BST a, left :: BST a, right :: BST a} deriving Show

convert :: BST a -> Int
convert BSTNil = 0
convert a = (-1)

insertElem :: (Ord a, Eq a) => BST a -> a -> BST a
insertElem tree node = auxf3 tree node (-1)

auxf3 BSTNil node _ = BST (BSTNode node) BSTNil BSTNil
auxf3 tree node x
		| and [(convert $ root tree) == 0, x == 0] = (BST (BSTNode node) BSTNil BSTNil)
		| and [(convert $ root tree) == 0, x == 1] = (BST (BSTNode node) BSTNil BSTNil)
		| (element $ root tree) > node = (tree {left = (auxf3 (left tree) node 0)})
		| (element $ root tree) < node = (tree {right = (auxf3 (right tree) node 1)})
		

findElem :: (Ord a, Eq a) => BST a -> a -> Maybe a
findElem BSTNil _ = Nothing
findElem tree node = auxf4 tree node

auxf4 BSTNil _ = Nothing
auxf4 tree node
		| (element $ root tree) == node = Just node
		| (element $ root tree) > node = (auxf4 (left tree) node)
		| (element $ root tree) < node = (auxf4 (right tree) node)

subTree :: (Ord a, Eq a) => BST a -> a -> a -> Maybe (BST a)
subTree BSTNil _ _ = Nothing
subTree tree node1 node2 = if length value == 0 then Nothing else head value
					where value = (auxf5 tree node1 node2)
auxf5 _ x y
		| x >= y = [] 
auxf5 BSTNil _ _ = []
auxf5 tree node1 node2 
		| and [(convert $ left tree) == 0, (convert $ right tree) == 0] = []
		| and [(element $ root $ left tree) == node1, (element $ root $ right tree) == node2] = [Just tree]
		| otherwise = (auxf5 (left tree) node1 node2) ++ (auxf5 (right tree) node1 node2)  
	
inorder :: BST a -> [a]
inorder tree = auxf6 tree []

auxf6 BSTNil [] = []
auxf6 tree list = (auxf6 (left tree) list) ++ [(element $ root tree)] ++ (auxf6 (right tree) list)

{-
	Daca se poate as dori o rezolvare a acestui lab pentru partea de subTree.
-}

check3 :: TestData
check3 = do
  let root = foldl insertElem BSTNil [7, 4, 12, 2, 3, 1, 10, 15, 8]
      values = [1, 2, 3, 4, 7, 8, 10, 12, 15]
      tree1 = subTree root 1 3 
      tree2 = subTree root 4 5
      tree3 = subTree root 7 15
      tree4 = subTree root 1 8
  tests 3 4 $ 
          [ testVal "findElem" 0.5 (findElem root 3) (Just 3)
          , testVal "findElem" 0.5 (findElem root 5) Nothing
          , testSet "inorder" 1 (inorder root) values
          , testSet "subTree" 0.5 (inorder (fromJust tree1)) [1, 2, 3]
          , testCond "subTree" 0.5 (isNothing tree2)
          , testSet "subTree" 0.5 (inorder (fromJust tree3)) values
          , testSet "subTree" 0.5 (inorder (fromJust tree4)) values]

{-
  4. (BONUS, 2p) Arbore multicăi nevid
  Având dat tipul Tree a, definiți funcționala analoagă lui map, care să
  aplice o funcție asupra cheilor nodurilor din arbore, și o funcțională
  analoagă lui foldl care să parcurgă nodurile în ordinea: rădăcină, copil_1,
  copil_2, ... copil_n. 
-}

data Tree a = Node
  { value :: a
  , children :: [Tree a]
  } deriving (Eq, Show)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree = undefined

foldlTree :: (b -> a -> b) -> b -> Tree a -> b
foldlTree = undefined

check4 :: TestData
check4 = do
  let tree = Node 1 [Node 2 [Node 3 []], Node 4 [Node 5 [], Node 6 [Node 7 [], Node 8 []]]]
  tests 4 2 $
          [ testSet "foldlTree" 0.5 (foldlTree (flip (:)) [] tree) [1..8]
          , testVal "foldlTree" 0.5 (foldlTree (+) 0 tree) 36
          , testVal "mapTree" 1 (foldlTree (+) 0 $ mapTree (*2) tree) 72]

{-
  5. (BONUS, 3p) Difference lists
  Se cere definirea tipului de date "difference list".

  Un difference list este o listă "parțial construită", i.e. ale cărei
  elemente din coadă nu sunt (neapărat) în întregime cunoscute. De
  exemplu, ne putem imagina existența unei liste:

  1 : (2 : (3 : xs)) = [1,2,3] ++ xs

  unde xs nu are (la momentul construirii) o valoare cunoscută.

  În limbajele funcționale putem modela difference lists folosindu-ne de
  închideri: putem privi o astfel de structură ca pe o funcție care
  așteaptă un parametru (o listă) și întoarce o listă. Exemplul anterior
  poate fi astfel exprimat în funcție drept următoarea listă:

  (\ xs -> [1,2,3] ++ xs)

  Observație: Care este tipul lambda-ului de mai sus?

  Avantajul acestei abordări este că permite efectuarea oricărei
  operație de adăugare în listă (e.g. concatenarea cu o altă listă) în
  O(1), cu dezavantajul că eliminarea este în general mai puțin eficientă,
  deoarece presupune evaluarea elementelor structurii.

  Se cere, mai concret:
  - Definirea ADT-ului difference list (DList), „împăturit peste” o
    funcție de tipul [a] -> [a] (e.g. folosind construcția newtype)
  - Conversia [a] -> DL a (dlFromList) și invers (dlToList)
  - Lista vidă (emptyDL), adăugarea în capul unei liste (consDL) și în
    coada ei (snocDL)
  - Concatenarea a două liste (appendDL)
  - Operații de eliminare: primul element (headDL) și coada (tailDL)
    unei liste

  Operațiile de lucru cu difference lists (cu excepția celor de
  eliminare) vor fi implementate cât mai eficient posibil, i.e. fără a
  folosi dlFromList și dlToList.

  Pentru mai multe detalii, consultați link-ul:
  https://wiki.haskell.org/Difference_list
-}
newtype DList a = DL a -- de rafinat tipul argumentului lui DL

dlFromList :: [a] -> DList a
dlFromList = undefined

dlToList :: DList a -> [a]
dlToList = undefined

emptyDL :: DList a
emptyDL = undefined

consDL :: a -> DList a -> DList a
consDL = undefined

snocDL :: a -> DList a -> DList a
snocDL = undefined

appendDL :: DList a -> DList a -> DList a
appendDL = undefined

headDL :: DList a -> Maybe a
headDL = undefined

tailDL :: DList a -> Maybe (DList a)
tailDL = undefined

check5 :: TestData
check5 = do
  tests 5 3 $
          [ testCond "toList, fromList" 1 $ dlToList (dlFromList "Ana are mere") == "Ana are mere"
          , testCond "cons, empty" 1 $ dlToList (consDL 1 $ consDL 2 $ consDL 3 emptyDL) == [1,2,3]
          , testCond "snoc, empty" 1 $ dlToList (snocDL 1 $ snocDL 2 $ snocDL 3 emptyDL) == [3,2,1]
          , testCond "append" 1 $ dlToList (dlFromList [1,2,3] `appendDL` dlFromList [4,5,6]) ==
                                            [1,2,3,4,5,6]
          , testCond "head, tail" 2 $ case tailDL (dlFromList [1,2,3,4,5]) of
                                      Just dl -> case tailDL dl of
                                                 Just dl -> headDL dl == Just 3]

{-
Helpers for testing :) You can also run check1, check2 etc independently
-}
check = quickCheck [check1, check2, check3, check4, check5]
