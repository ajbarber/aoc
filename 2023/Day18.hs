{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Day18 where

import Prelude
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.Char
import Control.Monad.State
import Debug.Trace
import Data.Maybe
import Data.MemoTrie
import Data.List

type Move = (String, String)
type Graph = M.Map (Int,Int) String
type Graph2 = V.Vector (V.Vector String)

data GraphState = GraphState { flipper :: Bool, graph :: Graph } deriving Show

main :: IO ()
main = do
  str <- readFile "Day18.txt"
  let moves = parseLine <$> lines str
  print moves
  let graph' = fst $ foldl move (M.singleton (0,0) "#", (0,0)) moves
      (m,n) = max' graph'
      (m0,n0) = min' graph'
  ---print $ bfs graph' (m0-1,n0-1)
  --print $ show (m0,m) <> show (n0, n)
  --print $  length (nub $ sort $ bfs graph' (m0-1,n0-1))
  let graph2 = toVectors graph'
  print graph2
  print $ bfs graph2 (m0-1,n0-1) (m0-1, n0-1)
  let externals = length (nub $ sort $ bfs graph2 (m0-1,n0-1) (m0-1,n0-1)) - 2*((1+m-m0) + (1+n-n0)) - 4
  let orig = abs (1 + m - m0) * abs (1 + n - n0)
  let rest = orig - externals
  -- let part1 = execState fill GraphState { flipper = False, graph = graph' }

  print orig
  print externals
  print rest

parseLine :: String -> Move
parseLine str = let (y:z:zs) = words str in (y,z)

toVector :: Graph -> Int -> V.Vector String
toVector graph row = let
  (_,n) = max' graph
  (_,n0) = min' graph in
  foldr (\a b -> case M.lookup (row, a) graph of
                   Just x -> V.cons x b
                   Nothing -> V.cons "." b) V.empty [n0-1..n+1]

toVectors :: Graph -> Graph2
toVectors graph = let
  (m,_) = max' graph
  (m0,_) = min' graph in
  foldr (V.cons . toVector graph) V.empty [m0-1..m+1]

positions :: (Int, Int) -> Move -> ([(Int, Int)], (Int, Int))
positions (j,k) (d, n) = let n' = read n in case d of
  "R" -> ([(j,k') | k' <- [k+1..k+n']], (j,k+n'))
  "L" -> ([(j,k') | k' <- [k-n'..k-1]], (j,k-n'))
  "D" -> ([(j',k) | j' <- [j+1..j+n']], (j+n',k))
  "U" -> ([(j',k) | j' <- [j-n'..j-1]], (j-n',k))

move :: (Graph , (Int, Int)) -> Move -> (Graph, (Int, Int))
move (g,curPos) move = (foldr (`M.insert` "#") g coords, last)
  where
    (coords, last) = positions curPos move

max' :: Graph -> (Int, Int)
max' g = (maximum $ fst <$> M.keys g, maximum $ snd <$> M.keys g)

min' :: Graph -> (Int, Int)
min' g = (minimum $ fst <$> M.keys g, minimum $ snd <$> M.keys g)

dimensions :: Graph2 -> (Int, Int)
dimensions g = (V.length g, V.length (g V.! 0))

bfs :: Graph2 -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
bfs graph offset (i,j) = go graph offset [(i,j)] (S.singleton (i,j))
  where
    go g o [] visited = S.toList visited
    go g o (q:qs) visited = bfsMemo g o qs visited q
    bfsMemo g o qs visited = memo \q -> let n = neighbours g offset q in go g o ((qs <> n) \\ S.toList visited) (foldr S.insert visited [q])

neighbours :: Graph2 -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
neighbours g (i0, j0) (i,j) =
  let (m,n) = dimensions g
      i' = i-i0
      j' = j-j0
      nbrs = [(i+1, j), (i-1, j), (i, j+1), (i, j-1)] in
      filter (\(p,q) -> g V.! (p-i0) V.! (q-j0) == ".") $ filter (\(k,l) -> k < m+i0 && k >= i0 && l >= j0 && l< n+j0) nbrs

fill :: State GraphState Int
fill = do
  gs0 <- get
  let (m,n) = max' gs0.graph
  let (m0,n0) = min' gs0.graph
  forM_ [m0..m] \i -> do
    modify (\gs -> gs { flipper = True})
    forM_ [n0..n] \j -> fillRow (i,j)
  --  modify (\gs -> gs { flipper = True})
  --  forM_ (reverse [n0..n]) \j -> fillRow (i,j)
  gs1 <- get
  let externals = M.size $ M.filter (`elem` ["*","#"]) gs1.graph
  let rest = abs (1 + m - m0) * abs ( 1 + n - n0) - externals
  pure rest

fillRow :: (Int, Int) -> State GraphState ()
fillRow (i,j) = do
  gs@GraphState{..} <- get
  when (M.lookup (i,j) graph == Just "#") $ put gs { flipper = not flipper }
  GraphState{..} <- get
  when (flipper && isNothing (M.lookup (i,j) graph)) $ modify (\gs -> gs { graph = M.insert (i,j) "*" graph })
