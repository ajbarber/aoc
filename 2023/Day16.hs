{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Day16 where

import Prelude
import qualified Data.Map as M
import Data.Functor.Identity
import Control.Monad
import Control.Monad.Extra
import Data.Function
import Data.Either
import Control.Monad.State
import Debug.Trace
import Data.List

type Coords = (Int, Int)

data Direction = U | D | R | L deriving (Show, Eq, Ord)

type Maze = M.Map (Int, Int) Char

-- Represents current coordinates and direction
data Tracer = Tracer Coords Direction deriving (Show, Eq, Ord)

data MazeState = MazeState { visited :: [Coords], maze :: Maze, tracers :: [Tracer] } deriving (Show)

main :: IO ()
main = do
  str <- readFile "Day16.txt"
  let m = foldr (uncurry toMap) M.empty $ zip [0..] (lines str)
  --print m
  let part1 =evalState loopStep MazeState { visited= [(0,0)], maze=m, tracers= [Tracer (0, 0) D1]}
  print part1

toMap :: (Ord a, Enum a', Ord a', Num a') => a -> [b] -> M.Map (a,a') b -> M.Map (a,a') b
toMap i s m = foldr (\(j,a) b -> M.insert (i,j) a b) m (zip [0..] s)

-- step :: [Tracer] -> Maze -> Maze
-- step tracers m =

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
  pure $ trace (show a) if length a > 10 && all (==head a) (take 20 a)
  then Right (head a)
  else Left (cur:a)) [0]

-- step algorithm
-- if current square . increment tracer coords in same direction , same with pointy end of splitter
-- if current square | split off two tracers if in 90 deg angle
-- \/ 90degree turn in one dir
--
step :: State MazeState Int
step = do
  ms@MazeState {..} <- get
  let tracers' = stepTracers maze tracers -- step tracers
  let visited' = (\(Tracer coords _) -> coords) <$> tracers'
  put $ ms { tracers = tracers', visited = visited <> visited' }
  pure (length $ nub $ sort (visited' <> visited))

stepTracers :: Maze -> [Tracer] -> [Tracer]
stepTracers maze tracers = flip concatMap tracers \(Tracer coords dir) ->
  let coords' = walk dir coords in
      case (M.lookup coords' maze, dir) of
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
