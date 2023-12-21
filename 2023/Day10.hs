{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Day10 where

import Prelude
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Trans.Writer.CPS
import Data.Foldable
import Data.List
import Debug.Trace
import Data.Function
import Data.Maybe

--- Read in to a Node = (Int,Int) String Map
--- BFS on nodes appending Node, Distance pairs
--- when all nodes visited, inspect output array, coalesce Node pairs with min(d1,s2)
--- find max distance Node

-- (row, col)
type Node = (Int, Int)

data Window = Window { lastDir:: Direction, toggle:: Bool } deriving (Show)

data Direction = NE | NW | SE | SW | V | H | Start | X | Internal | None deriving (Eq)

instance Show Direction where
  show NE = "L"
  show NW = "K"
  show SW = "7"
  show SE = "F"
  show V = "|"
  show H = "-"
  show Start = "S"
  show None = "."
  show Internal = "&"
  show X = "*"

type Graph = M.Map Node Direction

main :: IO ()
main = do
  str <- readFile "Day10.txt"
  let graph = execState (parse str) M.empty
  let start = head . M.keys $ M.filter (==Start) graph
  let loop = execWriter (walk start graph)
  let res = map (maximumBy (compare `on` snd)) (groupBy ((==) `on` snd) loop)
  let part1 = fst $ last res
  let init = Window { lastDir = None, toggle = False }
  let (rows, _) = dimensions graph
  -- Note: Need to replace S to determine interior/exterior status of cells in this row
  let graph' = replaceS start graph
  let loopNodes = snd <$> loop
  let part2 = concatMap (snd . flip execState (init,[]) . interiorNodes graph' loopNodes) [0..rows]
  print part1
  print part2
  --- Some pretty printing
  let graphWithLoop = execState (addPath X loopNodes >> addPath Internal part2) graph
  let str = execState (printPath graphWithLoop) ""
  writeFile "Day10Out.txt" str

toDirection :: Char -> Direction
toDirection 'J' = NW
toDirection 'L' = NE
toDirection '7' = SW
toDirection 'F' = SE
toDirection '|' = V
toDirection '-' = H
toDirection 'S' = Start
toDirection _ = None

parse ::  String -> State Graph ()
parse str = do
  forMWIndex_ (lines str) \(i, line) -> do
    forMWIndex_ line \(j, c) -> do
      modify (M.insert (i,j) (toDirection c))

forMWIndex_ :: (Monad m, Enum a0, Num a0) => [b1] -> ((a0, b1) -> m b) -> m ()
forMWIndex_ xs = forM_ (zip [0..] xs)

neighbours :: Graph -> Node -> [Node]
neighbours g n = execWriter (neighboursW g n)

joins :: Monad m => [Direction] -> Node -> Maybe Direction -> WriterT [Node] m ()
joins dirs node = traverse_ (\x -> when (x `elem` dirs) $ tell [node])

neighboursW :: Graph -> Node -> Writer [Node] ()
neighboursW g (i,j) = case M.lookup (i,j) g of
  Just V -> joinDown >> joinUp
  Just H -> joinLeft >> joinRight
  Just NW -> joinUp >> joinLeft
  Just NE -> joinUp >> joinRight
  Just SW -> joinDown >> joinLeft
  Just SE -> joinDown >> joinRight
  Just Start -> joinLeft >> joinRight >> joinUp >> joinDown
  _ -> pure ()
  where
    joinUp = joins [SE, SW, V] u' up
    joinDown = joins [NE, NW, V] d' down
    joinLeft = joins [NE,SE,H] l' left
    joinRight = joins [NW,SW,H] r' right
    l' = (i,j-1)
    r' = (i,j+1)
    d' = (i+1,j)
    u' = (i-1,j)
    left = M.lookup l' g
    right = M.lookup r' g
    down = M.lookup d' g
    up = M.lookup u' g

walk :: Node -> Graph -> Writer [(Int, Node)] ()
walk start graph = bfs graph [(0,start)] []
  where
    bfs _ [] _ = pure ()
    bfs graph (s:stack) visited = do
      let ns = neighbours graph (snd s) \\ visited
      tell [s]
      bfs graph (stack ++ ((fst s + 1,) <$> ns)) (visited++ns)

dimensions :: Graph -> (Int, Int)
dimensions graph = fst $ M.findMax graph

-- loop is represented as a [(Int, Int)].
-- Graph is a map of (Int,Int) to Direction
-- Part 2 algorithm outline
-- walk the graph L to R top to bottom
-- check if we hit a path at each node on walk
-- check if this is a vert, flip a bool indicating
-- we are on an interior area.
-- Count visited nodes when bool true and not on loop.
--  ...L7||...-|...
--  False True False
--- 000000123444444
interiorNodes :: Graph -> [(Int, Int)] -> Int -> State (Window, [(Int,Int)]) ()
interiorNodes graph path row =
    forM_ [(row,j)| j<- [0..jMax] ] (\c -> do
      (window, arr) <- get
      let isVertical = M.lookup c graph `elem` verticals
          verticals = [Just V, Just SE, Just NE, Just NW, Just SW]
          onPath = c `elem` path
          currentDir = fromMaybe None $ M.lookup c graph
          toggle' = if onPath && isVertical && not (edgeCase window.lastDir currentDir)
                    then not window.toggle else window.toggle
          window' = if onPath
                    then window { lastDir = if isVertical then currentDir else window.lastDir}
                    else window
          arr' = if window.toggle && not onPath && not (reachRhs c graph path)
                 then c:arr
                 else arr
      put (window' { toggle = toggle'}, arr'))
      where
        (iMax, jMax) = dimensions graph

-- Treat these occurring in succession as one crossing.
edgeCase :: Direction -> Direction -> Bool
edgeCase SE NW =True
edgeCase NE SW = True
edgeCase _ _ = False

reachRhs :: (Int, Int) -> Graph -> [(Int,Int)] -> Bool
reachRhs (i,j) g path = let (_, jMax) = dimensions g in
  all (`notElem` path) [(i,j+k) | k <- [1..jMax -j]]

replaceS :: Node -> Graph -> Graph
replaceS = M.adjust (const NW)

addPath :: Direction -> [(Int,Int)] -> State Graph ()
addPath dir path  = forM_ path \(i,j) -> do
  s <- get
  put $ M.insert (i,j) dir s

printPath :: Graph -> State String ()
printPath g =
  forM_ [0..iMax] \i -> do
     let line = foldr (\a b-> b <> show (fromMaybe None (M.lookup (i,a) g))) "" [0..jMax]
     modify (<> line <> "\n")
  where
     (iMax, jMax) = dimensions g
