{-# LANGUAGE BlockArguments #-}

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
--original graph before I realised a map was too inefficient
type Graph = M.Map (Int,Int) String
type Graph2 = V.Vector (V.Vector String)

data GraphState = GraphState { flipper :: Bool, graph :: Graph } deriving Show

main :: IO ()
main = do
  str <- readFile "Day18.txt"
  let moves = parseLine <$> lines str
  print moves
  -- part 1
  let pt1graph' = fst $ foldl move (M.singleton (0,0) "#", (0,0)) moves
      (m,n) = max' pt1graph'
      (m0,n0) = min' pt1graph'
  let pt1graph2 = toVectors pt1graph'
--  let externals = length (nub $ sort $ bfs pt1graph2 (m0-1,n0-1) (m0-1,n0-1)) - 2*(1+m-m0 + (1+n-n0)) - 4
--  let orig = abs (1 + m - m0) * abs (1 + n - n0)
--  let part1 = orig - externals

  --
  let pt2moves = parseLine2 <$> lines str
  let pt2coords = movesToCoords pt2moves
  --let pt2graph' = fst $ foldl' move (M.singleton (0,0) "#", (0,0)) pt2moves

 -- print orig
 -- print externals
 --3 print part1
  print pt2moves
  print pt2coords

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
    go g o (q:qs) visited = let n = neighbours g offset q in trace (show $ length visited) go g o (S.toList (S.fromList n S.\\ visited)<> qs) (foldr S.insert visited [q])

neighbours :: Graph2 -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
neighbours g (i0, j0) (i,j) =
  let (m,n) = dimensions g
      i' = i-i0
      j' = j-j0
      nbrs = [(i+1, j), (i-1, j), (i, j+1), (i, j-1)] in
      filter (\(p,q) -> g V.! (p-i0) V.! (q-j0) == ".") $ filter (\(k,l) -> k < m+i0 && k >= i0 && l >= j0 && l< n+j0) nbrs

hexChar :: Char -> Integer
hexChar ch = go (toUpper ch) where
  go ch
    | ch == '0' = 0
    | ch == '1' = 1
    | ch == '2' = 2
    | ch == '3' = 3
    | ch == '4' = 4
    | ch == '5' = 5
    | ch == '6' = 6
    | ch == '7' = 7
    | ch == '8' = 8
    | ch == '9' = 9
    | ch == 'A' = 10
    | ch == 'B' = 11
    | ch == 'C' = 12
    | ch == 'D' = 13
    | ch == 'E' = 14
    | ch == 'F' = 15
    | otherwise     = 0

parseHex :: String -> Integer
parseHex hxStr
    | not (null hxStr) = hexChar (last hxStr)+16*parseHex (init hxStr)
    | otherwise         = 0

direction :: String -> String
direction "0" = "R"
direction "1" = "D"
direction "2" = "L"
direction "3" = "U"

parseMoveInner :: String -> State (String, String) ()
parseMoveInner str = forM_ (zip [0..length str-1] str) \(i,a) -> do
  (s1, s2) <- get
  if i == length str - 2 then put (s1, [a])
  else when (i >= 2 && i< length str - 2) $ put (s1 <> [a], s2)

parseLine2 :: String -> Move
parseLine2 str = let (s1, s2) = execState (parseMoveInner str) ("","") in (direction s2, show $ parseHex s1)

-- we start at (0,0)
-- next instruction is
-- D 10000000, -> next coordinate is (0,1000000)
-- R 10000000, -> next coordinate is (1000000, 1000000)
-- L 5000000 -> next coordinate is (500000, 1000000) and so on
movesToCoords :: [(String, String)] -> [(Int, Int)]
movesToCoords = scanl' (flip moveToCoord) (0,0)

moveToCoord :: (String, String) -> (Int, Int) -> (Int, Int)
moveToCoord ("R", p) (m, n) = (m, n + read p)
moveToCoord ("L", p) (m, n) = (m, n - read p)
moveToCoord ("D", p) (m, n) = (m + read p, n)
moveToCoord ("U", p) (m, n) = (m - read p, n)

-- area of polygon bound by vertices  (x_1, 0), (x_2, 0), (x_1, y_1), (x_2, y_2)
-- = 1/2 * (y_2 - y_1) * (x_1 + x_2)
shoelaceFormula :: [(Int, Int)] -> Int
shoelaceFormula xs = go xs
   where
     go [(x1,y1)] =
     go (x1, y1):(x2, y2):_ = (y2 + y1) +
