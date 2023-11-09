module Maze where

import Control.Lens
import Data.Maybe

type Row = Int
type Col = Int
 -- assume that it'll perfect rectangle or square
type Maze = [[Bool]]
type Point = (Row, Col)
type Dir = (Row, Col)

data Tree = Node Point [Tree]
          deriving (Eq, Show)

fromMaze :: Point -> Maze -> Tree
fromMaze p mz = let ps = neighbors p mz
                    mz' = block p mz
                in Node p ((\p' -> fromMaze p' mz') <$> ps)

neighbors :: Point -> Maze -> [Point]
neighbors p mz = let ps = filter (/= Nothing) $ (\d -> move p d mz) <$> dirs
                 in fromJust <$> ps

dirs :: [Dir]
dirs = [up, down, left, right]

up, down, left, right :: Dir
up = (-1, 0)
down = (1, 0)
left = (0, -1)
right = (0, 1)

move :: Point -> Dir -> Maze -> Maybe Point
move (r, c) (r', c') mz = let (row, col) = (r + r', c + c')
                              (maxRow, maxCol) = (length mz, length (mz !! 0))
                          in if row < 0 || col < 0 || row >= maxRow || col >= maxCol
                             then Nothing
                             else if (mz !! row) !! col
                                  then Just (row, col)
                                  else Nothing

-- needed to avoid cycles
block :: Point -> Maze -> Maze
block (r, c) g = g & element r . element c .~ False

start :: Point
start = (0, 0)

maze :: Maze
maze = [ [True, True,  False]
       , [True, True,  True]
       , [True, False, True]
       ]
