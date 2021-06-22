module Baka.Grid where

import Control.Lens
import Data.Maybe
import Debug.Trace

data Point = Point Row Column
           deriving (Eq, Show)

type Grid = [[Bool]]

type Column = Int
type Row = Int
type Direction = (Row, Column)

-- paths from initial point
paths :: Point -> Grid -> [Point] -> [[Point]]
paths p g ps = let ns = neighbors p g
                   acc = ps <> [p]
               in case ns of
                    [] -> [acc]
                    _  -> concatMap (\n -> paths n (block p g) acc) ns

neighbors :: Point -> Grid -> [Point]
neighbors p g = fromJust <$> filter (/= Nothing) ((\d -> dir p d g) <$> directions)

dir :: Point -> Direction -> Grid -> Maybe Point
dir (Point r c) (r', c') g =
  let row = r + r'
      col = c + c'
      maxRow = length grid
      maxCol = length (grid !! 0)
  in if col < 0 || row < 0 || row >= maxRow || col >= maxCol
     then Nothing
     else if (g !! row) !! col
          then Just (Point row col)
          else Nothing

block :: Point -> Grid -> Grid
block (Point r c) g = g & element r . element c .~ False

directions :: [Direction]
directions = [left, right, up, down]

left :: Direction
left = (-1, 0)

right :: Direction
right = (1, 0)

up :: Direction
up = (0, -1)

down :: Direction
down = (0, 1)

start = Point 0 0
grid = [ [True, True,  False, False]
       , [True, False, False, False]
       , [True, True,  False, True]
       ]
