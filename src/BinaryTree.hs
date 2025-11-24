module BinaryTree where

data Tree a = Nil | Node a (Tree a) (Tree a)
  deriving (Eq, Show)

fromList :: Ord a => [a] -> Tree a
fromList = foldr add Nil
  where add :: Ord a => a -> Tree a -> Tree a
        add x Nil = Node x Nil Nil
        add x (Node y l r)
          | x == y    = Node y l r
          | x < y     = Node y (add x l) r
          | otherwise = Node y l (add x r)

level :: Tree a -> Int -> [a]
level Nil _          = []
level (Node x _ _) 0 = [x]
level (Node _ l r) n = level l (n-1) <> level r (n-1)

depth :: Tree a -> Int
depth Nil          = 0
depth (Node _ l r) = 1 + max (depth l) (depth r)

levels :: Tree a -> [[a]]
levels t = level t <$> [0..depth t - 1]

search :: Ord a => Tree a -> a -> Bool
search Nil _  = False
search (Node x l r) y
  | y == x    = True
  | y < x     = search l y
  | otherwise = search r y

preorder :: Tree a -> [a]
preorder Nil          = []
preorder (Node x l r) = [x] <> postorder l <> postorder r

inorder :: Tree a -> [a]
inorder Nil          = []
inorder (Node x l r) = postorder l <> [x] <> postorder r

postorder :: Tree a -> [a]
postorder Nil          = []
postorder (Node x l r) = postorder l <> postorder r <> [x]
