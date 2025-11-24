module Hr.Parens where

parens :: Int -> [String]
parens 0 = [""]
parens n = [ "(" <> x <> ")" <> y | i <- [0..n-1], x <- parens i, y <- parens (n-1-i) ]
