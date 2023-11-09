module RiverSkip where

type Velocity = Int
type River = String

data Tree = Node Bool Velocity [Tree]
          deriving (Eq, Show)

toTree :: River -> Bool -> Velocity -> Tree
toTree [] _ v = Node False v []
toTree r@(s:_) b v =
  case s of
    '*' -> Node True v ((\v' -> toTree (drop v' r) b v') <$> vs)
    _   -> Node False v []
  where vs = [ v + v' | v' <- [-1, 0, 1], v + v' > 0 ]

path :: Tree -> [Velocity] -> [Velocity]
path (Node b v ts) vs = if b
                        then let vs' = vs <> [v]
                             in concatMap (`path` vs') ts
                        else vs
