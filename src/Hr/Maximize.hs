module Hr.Maximize where

import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Streamly.Data.Stream as S
import qualified Streamly.Data.Fold as F
import qualified Streamly.Data.Unfold as U

prod :: [[Int]] -> [Int] -> [[Int]]
prod [] acc = [acc]
prod (xs:xss) acc = do
  x <- xs
  prod xss (acc <> [x])


prod' :: [[a]] -> [[a]]
prod' = foldr (\xs as -> xs >>= (<$> as) . (:)) [[]]

prod'' :: [[a]] -> [[a]]
prod'' = foldr (\xs as -> [ x : a | x <- xs, a <- as ]) [[]]
