module List where

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys         = ys
merge xs []         = xs
merge xs@(x':xs') ys@(y':ys')
  | x' < y'   = x':merge xs' ys
  | otherwise = y':merge xs ys'

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = let halves' = halves xs
               first   = fst halves'
               second  = snd halves'               
            in merge (msort first) (msort second)

halves :: [a] -> ([a], [a])
halves xs = let len = length xs `div` 2
            in (take len xs, drop len xs)

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = let left = [x' | x' <- xs, x' < x]
                   right = [x' | x' <- xs, x' >= x]
               in qsort left <> [x] <> qsort right
  
isSorted :: Ord a => [a] -> Bool
isSorted []     = True
isSorted [_]    = True
isSorted (x:y:ys) = if x < y
                    then True && isSorted (y:ys)
                    else False

bsort :: Ord a => [a] -> [a]
bsort []  = []
bsort [x] = [x]
bsort (x:y:ys) = if x < y
                 then x:(bsort (y:ys))
                 else y:(bsort (x:ys))
