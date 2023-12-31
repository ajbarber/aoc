{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Day17 where

import Prelude
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import Control.Monad
import Debug.Trace
import Control.Monad.State
import Data.Maybe
import Data.List
import Data.Char

main :: IO ()
main = do
  str <- readFile "Day17.txt"
  let gr = V.fromList (V.fromList  <$> lines str)
      state = StepState { distances = M.singleton (XCoords R 1 (0,0)) (Dist 0), path=Empty,
                          queue=[(Nothing,Node (XCoords R 1 (0,0)) Empty)] }
      paths = dijkstra gr state (m-1,n-1)
      (m,n) = dimensions gr
  print ("dimensions are " <> show (m,n))
  print paths

type Graph = V.Vector (V.Vector Char)
type Distances = V.Vector (V.Vector Int)
type Queue a = [a]
data Direction = R | L | U | D | None deriving (Eq, Show, Ord)
-- extended coordinates which holds onto direction,
-- number of consecutive moves in that direction,
-- and a as the underlying polymorphic coordinates
data XCoords a = XCoords Direction Int a deriving (Show, Functor, Eq, Ord)
-- wrap around a coordinate type to remember a path
data Path a = Node a (Path a) | Empty deriving (Eq, Show)

type XPath = Path (XCoords (Int, Int))

data Dist = Dist Int | Inf deriving (Eq, Show)

-- v is the point under consideration
data StepState = StepState { distances :: M.Map (XCoords (Int, Int)) Dist,
                             path :: XPath,
                             queue :: [(Maybe Int , XPath)] } deriving (Show)

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

strip :: Path (XCoords w) -> w
strip = coords . current

unDist :: Dist -> Int
unDist (Dist x) = x
unDist Inf = 1000

head' (x:xs) = Just x
head' [] = Nothing

mkCoords (XCoords dir n a) dir' = XCoords dir' (if dir == dir' || dir == None then n+1 else 1)

dijkstra :: Graph -> StepState -> (Int,Int) -> Dist
dijkstra graph state target = case head' state.queue of
  Just q -> dijkstra graph (state' (snd q)) target
  Nothing -> minimum $ M.elems $ M.filterWithKey (\k a -> coords k == target) state.distances
  where
    state' q = foldr (step graph q) state { queue = tail state.queue } (neighbours graph q)

update' :: [(Maybe Int, XPath)] -> (Maybe Int, XPath) -> [(Maybe Int, XPath)]
update' xs (i,coords) = sortOn fst ((i,coords): filter ((/=coords) . snd) xs)

step ::  Graph -> XPath -> XPath -> StepState -> StepState
step graph u v state  =
  let du = fromMaybe Inf (M.lookup u' distances)
      dv = fromMaybe Inf (M.lookup v' distances)
      duv = digitToInt ((graph V.! fst (coords v')) V.! snd (coords v'))
      StepState {..} = state
      v' = current v
      u' = current u
      alt = Dist duv <> du
  in if alt < dv then-- trace (show ("u=" <> show u' <>
                      --             "v=" <> show v' <>
                       --            "duv=" <> show duv <>
                       --           "du=" <> show du
                       --           ))
                   -- trace (show queue)
                              state { distances = M.insert v' alt distances,
                              queue = update' queue (Just (unDist alt), v),
                              path = u} else state

neighbours :: Graph -> XPath -> [XPath]
neighbours graph node@(Node cur@(XCoords dir n (i,j)) _) =
  let nbrs = case dir of
        R -> [u,d,r]
        L -> [u,d,l]
        D -> [l,r,d]
        U -> [l,r,u]
        None -> [l,r,u,d]
  in (`Node` node) <$> filter legal (filter (within . coords) nbrs)
  where
     (iMax, jMax) = dimensions graph
     d = mkCoords cur D (i+1,j)
     l = mkCoords cur L (i,j-1)
     r = mkCoords cur R (i,j+1)
     u = mkCoords cur U (i-1,j)
     legal n = travelled n <= 3
     within (x,y) = x>=0 && y>=0 && x <iMax && y<jMax
 -- in filterMap (\(x,y) -> x>=0 && y>=0 &&  x <iMax && y<jMax) nbrs

dimensions :: Graph -> (Int, Int)
dimensions g = (V.length g, V.length $ g V.! 0)
