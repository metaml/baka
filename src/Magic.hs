module Magic where

-- 3x3 squares

import Data.List ((\\), nub, permutations)
import Data.List.Extra (chunksOf)

squares :: [[[Int]]]
squares = chunksOf 3 <$> permutations [1..9]

row :: Int -> [[Int]] -> [Int]
row n xss = xss !! n

col :: Int -> [[Int]] -> [Int]
col n xss = [(xss !! 0) !! n, (xss !! 1) !! n, (xss !! 2) !! n]

rdiag :: [[Int]] -> [Int]
rdiag xss = [(xss !! 0) !! 0, (xss !! 1) !! 1, (xss !! 2) !! 2]

ldiag :: [[Int]] -> [Int]
ldiag xss = [(xss !! 0) !! 2, (xss !! 1) !! 1, (xss !! 2) !! 0]

msquares :: [[[Int]]] -> [[[Int]]]
msquares xss = nub [ xs
                   | xs <- xss
                   , r2 <- [0..2]
                   , r1 <- [0..2] \\ [r2]
                   , r0 <- ([0..2] \\ [r2]) \\ [r1]
                   , c2 <- [0..2]
                   , c1 <- [0..2] \\ [c2]
                   , c0 <- ([0..2] \\ [c2]) \\ [c1]
                   , sum (ldiag xs) == sum (rdiag xs)               
                   , sum (row r2 xs) == sum (row r1 xs)
                   , sum (row r1 xs) == sum (row r0 xs)
                  , sum (row r2 xs) == sum (rdiag xs)               
                   , sum (row r1 xs) == sum (rdiag xs)               
                   , sum (row r0 xs) == sum (rdiag xs)               
                   , sum (col c2 xs) == sum (col c1 xs)
                   , sum (col c1 xs) == sum (col c0 xs)
                   , sum (col c2 xs) == sum (rdiag xs)               
                   , sum (col c1 xs) == sum (rdiag xs)               
                   , sum (col c0 xs) == sum (rdiag xs)
                   , sum (col c2 xs) == sum (row r2 xs)
                   , sum (col c1 xs) == sum (row r1 xs)
                   , sum (col c0 xs) == sum (row r0 xs)                                      
                   ]
               
msquares' :: [[[Int]]] -> [[[Int]]]
msquares' xss = nub [ xs
                    | xs <- xss
                    , r2 <- [0..2]
                    , r1 <- [0..2] \\ [r2]
                    , r0 <- ([0..2] \\ [r2]) \\ [r1]
                    , c2 <- [0..2]
                    , c1 <- [0..2] \\ [c2]
                    , c0 <- ([0..2] \\ [c2]) \\ [c1]
                    , sum (ldiag xs) == 15
                    , sum (rdiag xs) == 15              
                    , sum (row r2 xs) == 15
                    , sum (row r1 xs) == 15
                    , sum (row r0 xs) == 15
                    , sum (col c2 xs) == 15
                    , sum (col c1 xs) == 15
                    , sum (col c0 xs) == 15
                    ]
               
