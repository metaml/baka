module Hr.Dna where

import Control.Monad (replicateM)
import Data.List (isPrefixOf)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Streamly.Data.Stream as S
import qualified Streamly.Data.Fold as F
import qualified Streamly.Data.Unfold as U

type Gene = String
type Health = Int
type Dna = String

-- @todo: represent as a fold
match :: Gene -> Dna -> [Bool]
match _ []           = []
match g dna@(_:dna') = if g `isPrefixOf` dna
                       then True:match g dna'
                       else False:match g dna'

health :: Gene -> Health -> Dna -> Int
health g h dna = let hs = (\hit -> if hit then h else 0) <$> match g dna
                 in sum hs

dnaMain :: IO ()
dnaMain = do
  n :: Int             <- getLine <&> read
  genes :: [String]    <- getLine <&> take n . words
  hps :: [String]      <- getLine <&> take n . words
  strands :: Int       <- getLine <&> read -- number of lines ("strands") of dna 
  rawdna :: [[String]] <- replicateM strands (getLine >>= pure . words)
  hs :: [Int] <- S.fromPure rawdna
                 & S.unfoldMany U.fromList  
                 & fmap (\es -> (read (es !! 0) :: Int, read (es !! 1) :: Int, es !! 2))
                 & fmap (\(start, end, dna) -> ( take (end - start + 1) . drop start $ zip genes hps
                                               , dna
                                               )
                        )
                 & fmap (\(ghs, dna) -> ((\(g, h) -> (g, read h :: Int)) <$> ghs, dna))
                 & fmap (\(ghs, dna) -> ((\(g, h) -> (g, h, dna)) <$> ghs))
                 & fmap (\ghds -> (\(g, h, d) -> health g h d) <$> ghds)
                 & fmap sum
                 & S.fold F.toList
  let maxh = maximum hs
      minh = minimum hs
  putStrLn $ show maxh
  putStrLn $ show minh  
  pure ()
