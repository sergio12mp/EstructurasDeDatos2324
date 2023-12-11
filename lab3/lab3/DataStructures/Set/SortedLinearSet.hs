-------------------------------------------------------------------------------
-- Linear implementation of Sets with nodes sorted according to values
-- and non-repeated elements
--
-- Data Structures. Grado en Informática. UMA.
--
-- STUDENT'S NAME:
-------------------------------------------------------------------------------

module DataStructures.Set.SortedLinearSet 
  ( Set
  , empty
  , isEmpty
  , size
  , insert
  , isElem
  , delete

  , fold

  , union
  , intersection
  , difference  
  ) where

import Data.List(intercalate)
import Test.QuickCheck
import GHC.Parser.Lexer (xset)

-- Invariants for this data structure:
--  * (INV1) All Nodes store different elements (no repetitions)
--  * (INV2) Nodes are sorted in ascending order with 
--           respect to values of their elements
--
-- An example of a well constructed set:
--   Node 2 (Node 5 (Node 8 Empty))
--
-- Examples of wrong sets:
--   Node 2 (Node 5 (Node 5 (Node 8 Empty))) -- REPETITION OF ELEMENT 5!
--   Node 7 (Node 1 (Node 8 Empty)) -- ELEMENTS NOT IN ASCENDING ORDER!

data Set a  = Empty | Node a (Set a)

empty :: Set a
empty = Empty

isEmpty :: Set a -> Bool
isEmpty Empty  = True
isEmpty _      = False

isElem :: (Ord a) => a -> Set a -> Bool
isElem x Empty = False
isElem x (Node y s)
  | x < y      = False
  | otherwise  = x==y || isElem x s


insert :: (Ord a) => a -> Set a -> Set a
insert x Empty = Node x Empty
insert x (Node y s)
  | x < y = Node x (Node y s)
  | x==y = Node y s
  | otherwise = Node y (insert x s)


delete :: (Ord a) => a -> Set a -> Set a
delete x Empty = Empty
delete x (Node y s)
  | x < y = Node y s
  | x == y = s
  | otherwise = Node y (delete x s)

size :: Set a -> Int
size Empty = 0
size (Node y s) = size s +1

fold :: (a -> b -> b) -> b -> Set a -> b
fold f z = fun
 where
  fun Empty       = z
  fun (Node x s)  = f x (fun s)

union :: (Ord a) => Set a -> Set a -> Set a
union s s'  = fold insert s s'

union2 ::(Ord a) => Set a -> Set a -> Set a
union2 Empty xs = xs
union2 xs Empty = xs
union2 s1@(Node x xs) s2@(Node y ys)
  |x == y = Node  x (union2 xs ys)
  |x>y = Node y (union2 s1 ys)
  |otherwise = Node x (union2 xs s2)

difference :: (Ord a) => Set a -> Set a -> Set a
difference s s' = fold delete s s'

intersection :: (Ord a) => Set a -> Set a -> Set a
intersection Empty xs = Empty
intersection xs Empty = Empty
intersection s1@(Node x xs) s2@(Node y ys)
  |x==y = Node x (intersection xs ys)
  |x<y = intersection xs s2
  |otherwise = intersection s1 ys


-- Showing a set
instance (Show a) => Show (Set a) where
  show s  = "SortedLinearSet(" ++ intercalate "," (strings s) ++ ")"
    where
      strings Empty       = []
      strings (Node x s)  = show x : strings s

-- Set equality
instance (Eq a) => Eq (Set a) where
  Empty      == Empty         = True
  (Node x s) == (Node x' s')  = x==x' && s==s'
  _          == _             = False

-- This instance is used by QuickCheck to generate random sets
instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary  = do
      xs <- listOf arbitrary
      return (foldr insert empty xs)

