module Day8 where

import Prelude
import Control.Monad.State
import Data.Char (digitToInt)
import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Foldable ( traverse_ )
import Data.Tuple

main :: IO ()
main = do
  str <- readFile "2022/Day8.txt"
  let grid = coords $ parse str
  print (toNum $ flip execState M.empty $ solveS grid)
  let part2 = flip execState (initMap' (coords $ parse example))  $ seenViews grid
  print ("Max is " <> show (maximumBy (compare `on` snd) (M.toList part2)))

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

example :: String
example = "30373\n25512\n65332\n33549\n35390"

type Grid = [GridRow]
type GridRow = [((Int, Int), Int)]
type Table = M.Map (Int, Int) Bool
type Table2 = M.Map (Int, Int) Int

splitSeen :: Int -> [a] -> ([a], [a])
splitSeen i xs = go i [] xs
  where
    go 0 seen rest = (seen, rest)
    go i seen (x:xs) = go (i-1) (x:seen) xs

initMap :: (Int, Int) -> M.Map (Int, Int) Int
initMap (m,n) = M.fromList [((i,j),1) | i <- [0 .. m], j <- [0 .. n]]

initMap' :: Grid -> M.Map (Int, Int) Int
initMap' = initMap . bounds

bounds :: Grid -> (Int, Int)
bounds g = (length g - 1, length (head g) - 1)

seenViews :: Grid -> State Table2 ()
seenViews g = do
  traverse_ (viewHori g id) xs
  traverse_ (viewHori (transpose g) swap) xs
  where
    xs = [(i,j) | i <- [0 .. m], j <- [0 .. n]]
    (m, n) = bounds g

viewHori ::  Grid -> ((Int,Int) -> (Int, Int)) -> (Int,Int) -> State Table2 ()
viewHori g f (i,j) = do
  modify $ M.insertWith (*) (f (i,j)) (go (i,j))
  where
    xs = g !! i
    go (m,n) = let
      (l, (_,v):r) = splitSeen n xs
      right = viewLen v r
      left = viewLen v l in left*right

viewLen :: Int-> [(a,Int)] -> Int
viewLen v xs = length xs `min` (1 + length (takeWhile ((< v) . snd) xs))

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

scoreLeftS :: GridRow -> (Int, Integer)
scoreLeftS = foldl' (\(prev, b) ((i,j),a) -> (a, if a > prev then b + 1 else 1)) (0,0)

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
