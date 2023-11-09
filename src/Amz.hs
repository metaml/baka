module Amz where

import Data.List ((\\))

-- https://www.educative.io/blog/crack-amazon-coding-interview-questions#questions

-- [Int] is 1..n' with a missing number from 1..n'
-- missingNumber [3, 7, 1, 2, 8, 4, 5] = 6
missingNumber :: [Int] -> Int
missingNumber ns = let sum' = sum ns
                       n' = length ns + 1
                       sum'' = n' * (n' + 1) `div` 2
                   in sum' - sum''

-- [5, 7, 1, 2, 8, 4, 3] 10 = [(7, 3), (2, 8)]
sumOfTwo :: [Int] -> Int -> [(Int, Int)]
sumOfTwo ns s = let ps = [(x, y) | x <- ns, y <- ns \\ [x], x + y == s]
                in take (length ps `div` 2) ps

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xs@(x:xs') ys@(y:ys')
  | x <= y               = x:merge xs' ys
  | otherwise            = y:merge xs ys'

data Tree a = Empty | Node a (Tree a) (Tree a)
            deriving (Eq, Ord, Show)

toTree :: Ord a => [a] -> Tree a
toTree = foldr add Empty
  where add :: Ord a => a -> Tree a -> Tree a
        add a Empty = Node a Empty Empty
        add a (Node a' l r)
          | a == a'    = Node a' l r
          | a < a'     = Node a' (add a l) r
          | otherwise  = Node a' l (add a r)

levels :: Tree a -> [[a]]
levels t = map (level t) [0..depth t - 1]

level :: Tree a -> Int -> [a]
level Empty _          = []
level (Node a _ _) 0   = [a]
level (Node _ l r) lev = level l (lev - 1) <> level r (lev - 1)

depth :: Tree a -> Int
depth Empty = 0
depth (Node _ l r) = 1 + max (depth l) (depth r)

levels' :: Tree a -> [[a]]
levels' Empty        = repeat []
levels' (Node a l r) = [a]:zipWith (<>) (levels' l) (levels' r)

fib :: [Int]
fib = 0:1:zipWith (+) fib (tail fib)

fib' :: Int -> Int
fib' 0 = 0
fib' 1 = 1
fib' n = fib' (n-1) + fib' (n-2)

fac :: Int -> Int
fac 0 = 1
fac n = n*fac (n-1)

fac' :: Int -> Int
fac' n
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = n * fac (n - 1)

fac'' :: Int -> Int
fac'' 0 = 1
fac'' n = product [1..n]

facs :: Int -> [Int]
facs n = scanl (*) 1 [1..n]
