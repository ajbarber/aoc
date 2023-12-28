{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

module Day17 where

import Prelude
import qualified Data.Set as S
import qualified Data.Vector as V
import Control.Monad
import Debug.Trace

main :: IO ()
main = do
  str <- readFile "Day17.txt"
  let gr = V.fromList (V.fromList  <$> lines str)
  let paths = bfs gr S.empty [Node (XCoords R 1 (0,0)) Empty] (12,12)
  print paths

type Graph = V.Vector (V.Vector Char)
type Queue a = [a]
data Direction = R | L | U | D deriving (Eq, Show)
-- extended coordinates which holds onto direction,
-- number of consecutive moves in that direction,
-- and a as the underlying polymorphic coordinates
data XCoords a = XCoords Direction Int a deriving (Show, Functor)
-- wrap around a coordinate type to remember a path
data Path a = Node a (Path a) | Empty deriving Show

current (Node a _) = a

coords :: XCoords a -> a
coords (XCoords _ _ a) = a

strip :: Path (XCoords w) -> w
strip = coords . current

mkCoords (XCoords dir n a) dir' = XCoords dir' (if dir == dir' then n+1 else 1)

bfs :: Graph -> S.Set (Int,Int) -> [Path (XCoords (Int, Int))] -> (Int,Int) -> Path (XCoords (Int, Int))
bfs graph visited queue target = go graph visited queue
  where
    go g v (q@(Node (XCoords dir n c) _):qs)
      | c == target || null queue = q
      | otherwise = go g (foldr (S.insert . strip) v (neighbours g q)) (qs <> filter ((`S.notMember` v) . strip) (neighbours g q))

neighbours :: Graph -> Path (XCoords (Int,Int)) -> [Path (XCoords (Int,Int))]
neighbours graph node@(Node cur@(XCoords dir n (i,j)) _) =
  let nbrs = case dir of
        R -> [u,d,r]
        L -> [u,d,l]
        D -> [l,r,d]
        U -> [l,r,u]
  in (`Node` node) <$> filter (within . coords) nbrs
  where
     (iMax, jMax) = dimensions graph
     d = mkCoords cur D (i+1,j)
     l = mkCoords cur L (i,j-1)
     r = mkCoords cur R (i,j+1)
     u = mkCoords cur U (i-1,j)
     within (x,y) = x>=0 && y>=0 && x <iMax && y<jMax
 -- in filterMap (\(x,y) -> x>=0 && y>=0 &&  x <iMax && y<jMax) nbrs

dimensions :: Graph -> (Int, Int)
dimensions g = (V.length g, V.length $ g V.! 0)
