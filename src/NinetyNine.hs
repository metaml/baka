module NinetyNine where

import System.Random

-- 1: last element of a list
-- last
-- reverse xs !! 0
last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (_:xs) = last' xs

-- 2: last but one
-- last . init
-- reverse xs !! 1
last'' :: [a] -> a
last'' [] = error "empty list"
last'' [_] = error "singleton"
last'' [x, _] = x
last'' (_:xs) = last' xs

-- 3: kth element of a list
-- xs !! (k-1)
elem' :: [a] -> Int -> a
elem' [] _ = error "empty list or kth element doesn't exist"
elem' (x:_) 1 = x
elem' (_:xs) k = elem' xs (k-1)

-- 4: length of a list
-- foldl (\n _ -> n + 1) 0
-- foldr (\_ n -> n + 1) 0
len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs

-- 5: reverse a list
-- foldl (flip (:)) []
-- foldl (\a x -> x:a) []
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs <> [x]

-- 6: palindrome?
-- foldr (&&) True (zipWith (==) xs (reverse xs))
palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == reverse xs

-- 7: flatten a nested list
data NestedList a = Elem a | List [NestedList a]
                  deriving (Eq, Show)

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = flatten x <> flatten (List xs)

-- 8: eleminate duplicate consecutive elements in a list
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:x':xs) = if x == x'
                     then compress (x':xs)
                     else [x] <> compress (x':xs)

-- 9: pack consecutive duplicates
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = if x `elem` head (pack xs)
              then (x : head (pack xs)) : tail (pack xs)
              else [x] : pack xs

pack' :: Eq a => [a] -> [[a]]
pack' [] = []
pack' (x:xs) = (x:takeWhile (== x) xs):pack (dropWhile (== x) xs)

draw' :: Int -> [Int] -> IO [Int]
draw' 0 _  = return []
draw' _ [] = return []
draw' n ms = do
  r <- randomRIO (0, length ms - 1)
  let ms' = take r ms <> drop (r+1) ms
  ms'' <- draw' (n-1) ms'
  return ((ms !! r):ms'')
