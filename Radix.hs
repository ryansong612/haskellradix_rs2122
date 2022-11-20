module Radix where

import Prelude hiding (and, or)
import Data.Char

data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)
            deriving (Eq, Show)

type IntTree = Tree Int

data Bit = Zero | One
         deriving (Eq, Show)

type RadixTree = Tree Bit

type BitString = [Int]

--------------------------------------------------------------------------

buildIntTree :: [Int] -> IntTree
buildIntTree
  = foldr add Empty
  where
    add x Empty
      = Leaf x
    add x (Leaf y)
      = add x (Node y Empty Empty)
    add x t@(Node y l r)
      | x == y    = t
      | x < y     = Node y (add x l) r
      | otherwise = Node y l (add x r)

--------------------------------------------------------------------------

a, m :: Integer
m = 1073741824
a = 16387

rand :: Integer -> [Double]
rand s
  = fromInteger s / fromInteger m : rand s' where s' = (s * a) `mod` m

randomInts :: Int -> Int -> Integer -> [Int]
randomInts m n s
  = take m (map (round . (+1) . (* (fromIntegral n))) (rand s))

rs :: [Int]
rs = randomInts 1000 500 765539

--------------------------------------------------------------------------
-- Pre (universal): all integers are non-negative

sizeIT :: IntTree -> Int
sizeIT Empty    = 1
sizeIT (Leaf a) = 4
sizeIT (Node a lt rt) = sizeIT lt + 12 + sizeIT rt

sizeRT :: RadixTree -> Int
sizeRT (Leaf a) = 1
sizeRT (Node a lt rt) = sizeRT lt + 8 + sizeRT rt

--
-- NOTE: The above import Prelude hiding (and, or) 
-- will allow you to name these two functions without
-- a name clash
--
and :: Bit -> Bit -> Bit
and a b
  | a == Zero || b == Zero = Zero
  | otherwise              = One

or :: Bit -> Bit -> Bit
or a b
  | a == One || b == One = One
  | otherwise            = Zero

binary :: Int -> BitString
binary x
  = bin x []
  where
    bin 0 s = 0:s
    bin 1 s = 1:s
    bin x !s
      = bin q (r:s)
      where
        (q, r) = quotRem x 2

insert :: BitString -> RadixTree -> RadixTree
insert [] (Leaf x)
  = Leaf One

insert [] (Node x lt rt)
  = Node One lt rt

insert (i:is) (Node x lt rt)
  | i == 0    = Node x (insert is lt) rt
  | i == 1    = Node x lt (insert is rt)
  | otherwise = error "Invalid bit string: elements must be 0 or 1"

insert (i:is) (Leaf x)
  = insert (i:is) (Node x (Leaf Zero) (Leaf Zero))

buildRadixTree :: [Int] -> RadixTree
buildRadixTree [] = Leaf Zero
buildRadixTree ints
  = foldr (insert . binary) (Leaf Zero) ints


member :: Int -> RadixTree -> Bool
member x
  = member' (binary x)
  where
    member' [] (Leaf a)       = a == One
    member' [] (Node a lt rt) = a == One
    member' (i:is) (Leaf a)   = False
    member' (i:is) (Node a lt rt)
      | i == 0 = member' is lt
      | i == 1 = member' is rt


union :: RadixTree -> RadixTree -> RadixTree
union (Node a lt rt) (Leaf b) = Node (or a b) lt rt
union (Leaf a) (Node b lt rt) = Node (or a b) lt rt
union (Leaf a) (Leaf b)       = Leaf (or a b)
union (Node a lt rt) (Node b lt' rt') 
  = Node (or a b) (union lt lt') (union rt rt')

intersection :: RadixTree -> RadixTree -> RadixTree
intersection (Node a lt rt) (Leaf b) = Leaf (and a b)
intersection (Leaf a) (Node b lt rt) = Leaf (and a b)
intersection (Leaf a) (Leaf b)       = Leaf (and a b)
intersection (Node a lt rt) (Node b lt' rt')
  = Node (and a b) (intersection lt lt') (intersection rt rt')

-- CONCLUSION: The break-even point is 197.

-----------------------------------------------------------------------------
-- Some test trees...

figure :: RadixTree
figure
  = Node Zero (Leaf One)
               (Node One (Leaf Zero)
                          (Node One (Node Zero (Leaf One)
                                                 (Leaf Zero))
                                     (Leaf One)))

t1 :: IntTree
t1 = Node 20 (Node 8 Empty
                     (Node 12 Empty
                              Empty))
             Empty

t2 :: RadixTree
t2 = Node Zero (Node Zero (Leaf One)
                            (Node One (Leaf Zero) (Leaf One)))
                (Leaf One)