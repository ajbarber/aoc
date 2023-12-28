{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Day16 where

import Prelude
import qualified Data.Map.Strict as M
import Data.Functor.Identity
import Control.Monad
import Control.Monad.Extra
import Control.Parallel.Strategies (parMap, rseq)
import Data.Either
import qualified Data.Set as S
import Control.Monad.State.Strict
import Debug.Trace
import qualified Data.Vector as V
import Data.List
import Data.Maybe

type Coords = (Int, Int)

data Direction = U | D | R | L deriving (Show, Eq, Ord)

type Maze = V.Vector (V.Vector Char)

-- Represents current coordinates and direction
data Tracer = Tracer Coords Direction deriving (Show, Eq, Ord)

data MazeState = MazeState { visited :: S.Set (Coords,Direction), maze :: Maze, tracers :: [Tracer] } deriving (Show)

main :: IO ()
main = do
  str <- readFile "Day16.txt"
  let m = V.fromList $ map toVec (lines str)

  let startPos = perimeter (dimensions m)

  let (a, part1) = runState loopStep MazeState { visited=S.singleton ((0,0), D), maze=m, tracers= [Tracer (0, 0) D]}
  let part2 = parMap rseq (\t@(Tracer c d) -> runState loopStep MazeState { visited= S.singleton (c,d), maze=m, tracers=[t]}) startPos

  print ("Part 1 " <> show a)
  print ("Part 2 " <> show (maximum $ fst <$> part2))

toVec :: String -> V.Vector Char
toVec = V.fromList

dimensions :: V.Vector (V.Vector b) -> (Int, Int)
dimensions v = (length v, length $ v V.! 0)

-- rows/cols
perimeter :: (Int, Int) -> [Tracer]
perimeter (m,n) = [ Tracer (0,j) D | j <- [0..n] ] <>
                  [ Tracer (i,0) R | i <- [0..m] ] <>
                  [ Tracer (m,j) U | j <- [0..n] ] <>
                  [ Tracer (i,n) L | i <- [0..m] ]

walk :: Direction -> Coords -> Coords
walk dir (m,n) = case dir of
  R -> (m, n+1)
  L -> (m, n-1)
  U -> (m-1, n)
  D -> (m+1, n)

-- use the following exit condition, save the number of visited nodes in a list
-- and when we get 20 of them without change, we know we can exit safely
-- loopM does all the work for us
loopStep :: StateT MazeState Identity Int
loopStep = loopM (\a -> do
  cur <- step
  pure $ if length a > 5 && all (==head a) (take 20 a)
  then Right (head a)
  else Left  (cur:a)) [0]

-- step algorithm
-- if current square . increment tracer coords in same direction , same with pointy end of splitter
-- if current square | split off two tracers if in 90 deg angle
-- \/ 90degree turn in one dir
--
step :: State MazeState Int
step = do
  ms@MazeState {..} <- get
  let tracers' = stepTracers visited maze tracers -- step tracers
      visited' = (\(Tracer coords dir) -> (coords,dir)) <$> tracers'
      newVis = foldr S.insert ms.visited visited'
  put $ ms { tracers = tracers', visited = newVis }
  pure $ S.size (S.map fst newVis)

unTracer :: Tracer -> Coords
unTracer (Tracer coords dir) = coords

stepTracers :: S.Set (Coords,Direction) -> Maze -> [Tracer] -> [Tracer]
stepTracers visited maze tracers = flip concatMap tracers \t@(Tracer coords dir) ->
  let coords'@(m,n) = walk dir coords
      newTracers = case ( maze V.!? m >>= \x-> x V.!? n, dir) of
        (Just '.', dir) -> [Tracer coords' dir]
        (Just '/', U) -> [Tracer coords' R]
        (Just '/', L) -> [Tracer coords' D]
        (Just '/', R) -> [Tracer coords' U]
        (Just '/', D) -> [Tracer coords' L]
        (Just '\\', U) -> [Tracer coords' L]
        (Just '\\', L) -> [Tracer coords' U]
        (Just '\\', R) -> [Tracer coords' D]
        (Just '\\', D) -> [Tracer coords' R]
        (Just '-', R) -> [Tracer coords' R]
        (Just '-', L) -> [Tracer coords' L]
        (Just '-', D) -> [Tracer coords' L, Tracer coords' R]
        (Just '-', U) -> [Tracer coords' L, Tracer coords' R]
        (Just '|', D) -> [Tracer coords' D]
        (Just '|', U) -> [Tracer coords' U]
        (Just '|', L) -> [Tracer coords' U, Tracer coords' D]
        (Just '|', R) -> [Tracer coords' U, Tracer coords' D]
        (Nothing, _) -> [] -- hit an edge stay put
      in filter (\(Tracer c d) -> S.notMember (c,d) visited) newTracers
