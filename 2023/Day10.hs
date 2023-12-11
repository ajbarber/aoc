{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
module Day10 where

import Prelude
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Trans.Writer.CPS
import Data.Foldable
import Data.List
import Debug.Trace
import Data.Function

--- read in to a Node = (Int,Int) String Map
--- bfs on nodes appending Node, Distance pairs
--- when all nodes visited, inspect output array, coalesce Node pairs with min(d1,s2)
-- find max distance Node

-- (row, col)
type Node = (Int, Int)

data Direction = NE | NW | SE | SW | V | H | Start | None deriving (Show, Eq)

type Graph = M.Map Node Direction

main :: IO ()
main = do
  str <- readFile "Day10.txt"
  let graph = execState (parse str) M.empty
  let start = head . M.keys $ M.filter (==Start) graph
  let pairs = execWriter (walk start graph)
  let res = map (maximumBy (compare `on` snd)) (groupBy ((==) `on` snd) pairs)
  let part1 = fst $ last res
  print res
  print part1

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
