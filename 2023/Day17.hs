{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Day17 where

import Prelude
import qualified Data.Map as M
import qualified Data.Vector as V
import Control.Monad
import Data.Maybe
import Data.List
import Data.Char

main :: IO ()
main = do
  str <- readFile "Day17.txt"
  let gr = V.fromList (V.fromList  <$> lines str)
      start = XCoords R 1 (0,0)
      state = StepState { distances = M.singleton start (Dist 0),
                          queue = [(Nothing, start)] }
      paths = dijkstra gr state (m-1,n-1)
      (m,n) = dimensions gr
  print ("dimensions are " <> show (m,n))
  print ("Part 2" <> show paths)

type Graph = V.Vector (V.Vector Char)

type Distances = V.Vector (V.Vector Int)

type Queue a = [a]

data Direction = R | L | U | D | None deriving (Eq, Show, Ord)

-- extended coordinates which holds onto direction,
-- number of consecutive moves in that direction,
-- and a as the underlying polymorphic coordinates
data XCoords a = XCoords Direction Int a deriving (Show, Eq, Ord)

-- wrap around a coordinate type to remember a path
data Path a = Node a (Path a) | Empty deriving (Eq, Show)

type XPath = Path (XCoords (Int, Int))

data Dist = Dist Int | Inf deriving (Eq, Show)

-- v is the point under consideration
data StepState = StepState { distances :: M.Map (XCoords (Int, Int)) Dist,
                             queue :: [(Maybe Int , XCoords (Int, Int))] } deriving (Show)

instance Ord Dist where
    compare (Dist a) (Dist b) = compare a b
    compare (Dist _) Inf = LT
    compare Inf (Dist _) = GT
    compare Inf Inf = EQ

-- A semi group is all we need to add these things
instance Semigroup Dist where
  (Dist i1) <> (Dist i2) = Dist (i1+i2)
  Inf <> _ = Inf
  _ <> Inf = Inf

current (Node a _) = a

coords :: XCoords a -> a
coords (XCoords _ _ a) = a

travelled :: XCoords a -> Int
travelled (XCoords _ x _) = x

dirn :: XCoords a -> Direction
dirn (XCoords d _ _) = d

strip :: Path (XCoords w) -> w
strip = coords . current

unDist :: Dist -> Int
unDist (Dist x) = x
unDist Inf = 1000

head' (x:xs) = Just x
head' [] = Nothing

(&|&) :: (w0 ->Bool) -> (w0 -> Bool) -> (w0 -> Bool)
(&|&) = liftM2 (&&)

mkCoords (XCoords dir n a) dir' = XCoords dir' (if dir == dir' then n+1 else 1)

dijkstra :: Graph -> StepState -> (Int,Int) -> Dist
dijkstra graph state target = case head' state.queue of
  Just q -> dijkstra graph (state' (snd q)) target
  Nothing -> minimum $ M.elems $ M.filterWithKey (\k a -> coords k == target) state.distances
  where
    state' q = foldr (step graph q) state { queue = tail state.queue } (neighbours graph q)

update' :: Eq a => [(Maybe Int, XCoords a)] -> (Maybe Int, XCoords a) -> [(Maybe Int, XCoords a)]
update' xs (i,coords) = sortOn fst ((i,coords): filter ((/=coords) . snd) xs)

distLookup :: Ord a => M.Map a Dist -> a -> Dist
distLookup distances = fromMaybe Inf . flip M.lookup distances

step ::  Graph -> XCoords (Int,Int) -> XCoords (Int, Int) -> StepState -> StepState
step graph u v state  =
  let du = distLookup distances u
      dv = distLookup distances v
      duv = digitToInt ((graph V.! fst (coords v)) V.! snd (coords v))
      StepState {..} = state
      alt = Dist duv <> du
  in if alt < dv then state { distances = M.insert v alt distances,
                              queue = update' queue (Just (unDist alt), v)
                            } else state

neighbours :: Graph -> XCoords (Int, Int) -> [XCoords (Int, Int)]
neighbours graph cur@(XCoords dir n (i,j))  =
  let nbrs = case dir of
        R -> [u,d,r]
        L -> [u,d,l]
        D -> [l,r,d]
        U -> [l,r,u]
  in filter (minRuns cur &|& maxRuns &|& (within . coords)) nbrs
  where
     (iMax, jMax) = dimensions graph
     d = mkCoords cur D (i+1,j)
     l = mkCoords cur L (i,j-1)
     r = mkCoords cur R (i,j+1)
     u = mkCoords cur U (i-1,j)
     minRuns c n = (travelled c >= 4) || (dirn c == dirn n)
     maxRuns n = travelled n <= 10
     within (x,y) = x>=0 && y>=0 && x <iMax && y<jMax

dimensions :: Graph -> (Int, Int)
dimensions g = (V.length g, V.length $ g V.! 0)
