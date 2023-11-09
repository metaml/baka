module River where

type Velocity = Int
type Index = Int
type River = String

-- riv (-1) 2 "--****_"
data Tree = Nil | Branch Tree Tree Tree Velocity
          deriving (Eq, Show)

cross :: River -> Velocity -> Bool
cross r v = let (r', t) = toTree r v
            in length (filter (=='*') r') == depth t

toTree :: River -> Velocity -> (River, Tree)
toTree r v = let f = head r
                 l = last r
             in case (f, l) of
                  ('*', '*') -> (r, toTree' r v)
                  ('*', _)   -> let r' = "*" <> r in        (r', toTree' r' v)
                  (_  , '*') -> let r' = r <> "*" in        (r', toTree' r' v)
                  (_  , _)   -> let r' = "*" <> r <> "*" in (r', toTree' r' v)

toTree' :: River -> Velocity -> Tree
toTree' [] _     = Nil
toTree' r@(x:_) v = case x of
                      '*' -> Branch (if v-1 > 0 then toTree' (drop (v-1) r) (v-1) else Nil)
                                    (if   v > 0 then toTree' (drop v r) v         else Nil)
                                    (if v+1 > 0 then toTree' (drop (v+1) r) (v+1) else Nil)
                                    v
                      _   -> Nil

depth :: Tree -> Int
depth (Branch l m r _) = 1 + max (max (depth l) (depth m)) (depth r)
depth Nil              = 0
