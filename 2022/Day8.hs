module Day8 where

import Prelude
import Control.Monad.State
import Data.Char (digitToInt)
import Data.List
import qualified Data.Map as M
import Data.Foldable ( traverse_ )
import Data.Tuple

main :: IO ()
main = do
  str <- readFile "2022/Day8.txt"
  print (toNum $ flip execState M.empty $ solveS (coords $ parse str))

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

example :: String
example = "30373\n25512\n65332\n33549\n35390"

type Grid = [GridRow]
type GridRow = [((Int, Int), Int)]
type Table = M.Map (Int, Int) Bool

-- visible trees from LHS, append to our map with (||)
visibleLeft :: Table -> GridRow -> Table
visibleLeft m xs = snd $ foldl' (\(min,b) ((i,j), a) -> (a `max` min, M.insertWith (||) (i, j) (edge i || edge j || a > min) b)) (0,m) xs
  where
    edge i' = i' == 0 || i' == length xs - 1

solveHori :: Grid -> Table -> Table
solveHori g m = foldl visibleLeft m g

-- Take raw 2D List into our coordinate labeled @Grid
coords :: [[Int]] -> Grid
coords grid = map (\(row, elem) -> [((row,j), (grid !! row) !! j) | j <- [0..n - 1]]) (zip [0..m - 1] grid)
  where
    n = length . head $ grid
    m = length grid

solveEachEnd :: Grid -> Table -> Table
solveEachEnd grid m = solveHori (reverse <$> grid) (solveHori grid m)

-- to solve vertical, transpose the grid and solve horizontal problem
solve :: Grid -> Table -> Table
solve grid t = (solveEachEnd . transpose) grid $ solveEachEnd grid t

toNum :: Table -> Int
toNum table = length $ filter id (M.elems table)

---- using mtl

visibleLeftS :: GridRow -> State Table ()
visibleLeftS xs = do
  m <- get
  put $ snd $ foldl' (\(min,b) ((i,j), a) -> (a `max` min, M.insertWith (||) (i, j) (edge i || edge j || a > min) b)) (0,m) xs
  where
    edge i' = i' == 0 || i' == length xs - 1

-- | Solve each @GridRow from the LHS p.o.v
--   saving the table state as we go
solveHoriS :: Grid -> State Table ()
solveHoriS = traverse_ visibleLeftS

-- | Solve each @GridRow from the LHS and RHS p.o.v
solveEachEndS :: Grid -> State Table ()
solveEachEndS g = solveHoriS g >> solveHoriS (reverse <$> g)

-- | Solve from the Top and Bottom p.o.v by transposing
--   Then solve from the Left and Right hand sides. All results are
--   || 'ed together in visibleLeftS
solveS :: Grid -> State Table ()
solveS g = solveEachEndS g >> (solveEachEndS . transpose) g
