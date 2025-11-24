module Hr.Dir where

import Data.Text (Text, intercalate, splitOn)

canonicalDir :: Text -> Text
canonicalDir p = let es = splitOn "/" p
                     cs = canonize es []
                 in intercalate "/" cs

canonize :: [Text] -> [Text] -> [Text]
canonize [] acc = acc
canonize (h:hs) acc = case h of
                          "."  -> canonize hs acc
                          ".." -> if length acc > 1
                                  then canonize hs (init acc)
                                  else canonize hs acc 
                          _    -> canonize hs (acc <> [h])
