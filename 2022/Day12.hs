{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
module Day12 where

import Prelude

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.Strict
import Data.Maybe
import qualified Data.Vector as V
import Data.Char
import Debug.Trace
import qualified Data.Sequence as SQ
import qualified Data.Set as ST
import Test.QuickCheck

example :: String
example = "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi\n"

type Grid a = V.Vector (V.Vector a)

type Point = (Int, Int)

data Path a = Path a (Path a) | Nil deriving (Show, Functor)
data Value a =  Elevation a | Start | End  deriving (Eq, Ord, Show)

main :: IO ()
main = do
  text <- readFile "2022/Day12.txt"
  let parsed = parse text
  let start = fromJust $ execState (findV' parsed End) Nothing
  let end = fromJust $ execState (findV' parsed Start) Nothing
  let (m,n) = start
  print $ bfs Start parsed (SQ.singleton $ asPath start) (ST.singleton start)
  print $ bfs (Elevation 97) parsed (SQ.singleton $ asPath start) (ST.singleton start)

parse :: String -> Grid (Value Int)
parse str = fmap (fmap toElevation) . V.fromList $ V.fromList <$> lines str
  where
    toElevation c = case c of
      'S' -> Start
      'E' -> End
      z -> Elevation (ord z)

instance Functor Value where
  fmap _ Start = Start
  fmap f (Elevation a) = Elevation (f a)
  fmap _ End = End

(<&&>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<&&>) = liftA2 (&&)

(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) = liftA2 (||)

maybeZip :: [a] -> (a -> Maybe b) -> [(a,b)]
maybeZip xs f = foldr (\a b -> case f a of
                          Just x -> (a,x):b
                          Nothing -> b) [] xs

-- Elevation 13 *--->* Elevation (12,13,14,15,16)
neighbours :: Value Int -> Point -> Grid (Value Int) -> ST.Set Point -> [Point]
neighbours target loc@(m,n) grid visited = case val loc grid of
  Just now -> fmap fst . filter (((<=) (pred <$> now) . snd) <||> last' now <||> first' now) $ maybeZip xs $ flip val grid
  Nothing -> []
  where
    last' n = (const (n == End) <&&> (<=) (Elevation 121)) . snd
    first' n = (const (n <= Elevation 98) <&&> (==) target) . snd
    xs = [ adjacent | (dx,dy) <- [(0,1), (-1,0), (1,0), (0,-1)],
           let adjacent = (m + dy, n + dx),
           adjacent `ST.notMember` visited ] -- this filters

bounds :: Grid (Value a) -> (Int, Int)
bounds g = (V.length g, V.length (V.head g))

-- some fake imperative find code for fun
findV' :: Eq a => V.Vector (V.Vector (Value a)) -> Value a -> State (Maybe Point) ()
findV' g v =
  forM_ [0..m-1] $ \i ->
     forM_ [0..n-1] $ \j -> do
       when (val (i,j) g == Just v) $ put (Just (i,j))
  where
    (m,n) = bounds g

-- V.!? is O(1)
val :: Point -> Grid (Value a) -> Maybe (Value a)
val (m,n) g = g V.!? m >>= flip (V.!?) n

asPath :: Point -> Path (Point,Int)
asPath p = Path (p,0) Nil

-- bfs
-- take a grid, queue
-- check if queue is empty
-- if empty then return accumulated result (here just an Int, path length)
-- else pop queue and visit that node
-- add this nodes' neighbours to queue and recurse
-- Key point of BFS, you mark visited *AS YOU ENQUEUE*
-- !!First point must be labelled as visited!
-- SQ.>< is O(log(min(n1,n2))
bfs :: Value Int -> Grid (Value Int) -> SQ.Seq (Path (Point, Int)) -> ST.Set Point -> Path (Point, Int)
bfs target grid q@(h@(Path (v,pt) prev) SQ.:<| rest) visited
  | val v grid == Just target = h
  | otherwise = bfs target grid (rest SQ.>< SQ.fromList (fmap (`Path` h) (,pt+1) <$> ns')) (foldr ST.insert visited ns')
  where
    ns' = neighbours target v grid visited
bfs target grid _ _ = Nil

--- Quick check testing
--
--Sabqponm
--abcryxxl
--accszExk
--acctuvwj
--abdefghi
--

testGrid :: Grid (Value Int)
testGrid = parse example

prop_grid :: Bool
prop_grid = let f@(Path (p,s) _) = bfs Start testGrid (SQ.singleton $ asPath (2, 5)) (ST.singleton (2,5)) in trace (show f) s == 31

propNeighbours :: Bool
propNeighbours = neighbours Start (2,0) testGrid ST.empty == [(3,0), (2,1), (1,0)]
