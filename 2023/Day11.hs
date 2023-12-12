{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
module Day11 where

import Prelude
import Control.Monad.State.Strict
import qualified Data.Vector as V
import Data.Foldable
import Data.Char
import Data.Vector (findIndex)
import Data.List
import Data.Maybe
import Control.Monad.Loops

main :: IO ()
main = do
  str <- readFile "Day11.txt"
  print str
  let (graph, _) = runState (numbers (parseLines str)) []
  let g1 = runWithDups 1 graph
  let g100 = runWithDups 99 graph
  let g200 = runWithDups 199 graph
  let g1number = execState (numbers' g1) []
  let g100number = execState (numbers' g100) []
  let g200number = execState (numbers' g200) []
  let g1' = map (uncurry manhattan) (pairs g1number)
  let g100' = map (uncurry manhattan) (pairs  g100number)
  let g200' = map (uncurry manhattan) (pairs  g200number)
  let delta = zipWith (-) g200' g100'
  let part1 = sum g1'
  let part2 = sum $ zipWith (+) g100' ((9999*) <$> delta)
  print part1
  print part2

type Graph a = V.Vector (V.Vector a)

runWithDups :: Int -> Graph String -> Graph String
runWithDups n graph = let g' = foldr (flip (insertRow n)) graph rs
                          g'' = foldr (flip (insertCol n)) g' cs
                          (rs,cs) = blanks graph in g''

parseLines :: String -> Graph Char
parseLines str = V.fromList $ map V.fromList (lines str)

forWIndex_ x = for_ (zip [0..] x)

numbers :: V.Vector (V.Vector Char) -> State [Int] (V.Vector (V.Vector String))
numbers = mapM number

-- 2nd pass over the expanded vector to grab an assoc of coordinates and points
numbers' :: V.Vector (V.Vector String) -> State [(Int, (Int, Int))] ()
numbers' xs = mapM_ (uncurry number') $ V.zip (V.fromList [0..V.length xs -1]) xs

blanks :: Graph String -> ([Int],[Int])
blanks g = let
  c = filter (emptyVec (/=".") . col g) [0..cols]
  r = filter (emptyVec (/= ".") . row g) [0..rows] in (r, c)
  where
    (rows, cols) = dims g

-- (num rows, num cols)
dims :: Graph a -> (Int, Int)
dims g = (V.length g - 1, V.length (g V.! 0) - 1)

number' :: Int -> V.Vector String -> State [(Int,(Int,Int))] ()
number' i vec = do
  mapM_ (\(j,e) -> do
    xs <- get
    let ctr = if null xs then 0 else fst $ head xs
    when (e /= ".") $ put $ (ctr+1,(i,j)):xs) (V.zip (V.fromList [0..V.length vec - 1]) vec)

number :: V.Vector Char -> State [Int] (V.Vector String)
number vec = do
  mapM (\e -> do
    xs <- get
    let ctr = if null xs then 0 else head xs
    if e == '#' then do
      put $ (ctr+1):xs
      pure (show (ctr + 1))
    else pure ".") vec

col :: Graph a -> Int -> V.Vector a
col g i = V.map (V.! i) g

row :: Graph a -> Int -> V.Vector a
row g i = g V.! i

emptyVec :: (a-> Bool) -> V.Vector a -> Bool
emptyVec f v = null (V.filter f v)

insert_ :: Int -> V.Vector a -> Int -> a -> V.Vector a
insert_ n v j s = let before = V.ifilter (\i a -> i <=j) v
                      after = V.ifilter (\i a -> i > j) v
                      single = V.replicate n s in
                before V.++ single V.++ after

insertCol :: Int -> Graph String -> Int -> Graph String
insertCol n g i = V.map (\v -> insert_ n v i ".") g

insertRow :: Int -> Graph String -> Int -> Graph String
insertRow n g i = insert_ n g i (V.replicate (rlen + 1) ".")
  where
    (rlen, _) = dims g

pairs :: (Ord a, Ord b) => [(a,b)] -> [((a,b), (a,b))]
pairs l = sortOn snd $ [(x, y) | (x:ys) <- tails l, y <- l, fst x < fst y]

manhattan :: (a4, (Int, Int)) -> (a3, (Int, Int)) -> Int
manhattan (_,(si,sj)) (_,(di,dj)) = abs (di - si) + abs (dj - sj)

-----------------------------------------------------------------------------
-- Gets coordinates (row, col) of vector value c -- unused now that I store
-- the coords with the value in the execState to save lookup in the Vectors
find_ :: (Eq a, Enum t, Num t) => Graph a -> a -> Maybe (t, Int)
find_ g c = msum $ zipWith (\a b -> (a,) <$> b) [0..] (V.toList $ V.findIndex (== c) <$> g)

findAlt :: Eq a => Graph a -> a -> Maybe (Int, Int)
findAlt g c = let (f,s) = runState (find__ g c) 0 in Just (f,s)

find__ :: Eq a => Graph a -> a -> State Int Int
find__ g c = untilJust do
  i <- get
  modify (+1)
  pure $ V.findIndex (==c) (g V.! i)
------------------------------------------------------------------------------
