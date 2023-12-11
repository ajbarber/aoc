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

main :: IO ()
main = do
  str <- readFile "Day11.txt"
  print str
  let (graph, number) = runState (numbers (parseLines str)) 0
  let g100 = runWithDups 99 graph
  let ps =  pairs [1..number]
  print "Here"
  let g100' = mapMaybe (uncurry (manhattan g100)) ps

  let delta10 n = (\x-> x `mod` 100 + (x `div` 100) * n) <$> g100'
  let part2 = sum $ delta10 1000000
  print (part2)

type Graph a = V.Vector (V.Vector a)

runWithDups :: Int -> Graph String -> Graph String
runWithDups n graph = let g' = foldr (flip (insertRow n)) graph rs
                          g'' = foldr (flip (insertCol n)) g' cs
                          (rs,cs) = blanks graph in g''

parseLines :: String -> Graph Char
parseLines str = V.fromList $ map V.fromList (lines str)

forWIndex_ x = for_ (zip [0..] x)

numbers :: V.Vector (V.Vector Char) -> State Int (V.Vector (V.Vector String))
numbers = mapM number

blanks :: Graph String -> ([Int],[Int])
blanks g = let
  c = filter (emptyVec (/=".") . col g) [0..cols]
  r = filter (emptyVec (/= ".") . row g) [0..rows] in (r, c)
  where
    (rows, cols) = dims g

-- (num rows, num cols)
dims :: Graph a -> (Int, Int)
dims g = (V.length g - 1, V.length (g V.! 0) - 1)

number :: V.Vector Char -> State Int (V.Vector String)
number vec = do
  mapM (\e -> do
    ctr <- get
    if e == '#' then do
      modify (+1)
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

-- Gets coordinates (row, col) of vector value c
find_ :: (Eq a, Enum t, Num t) => Graph a -> a -> Maybe (t, Int)
find_ g c = msum $ zipWith (\a b -> (a,) <$> b) [0..] (V.toList $ V.findIndex (== c) <$> g)

pairs :: [Int] -> [(String, String)]
pairs l = [(show x, show y) | (x:ys) <- tails l, y <- l, x < y]

manhattan :: Eq a => Graph a -> a -> a -> Maybe Int
manhattan g src dst = do
  (si, sj) <- find_ g src
  (di, dj) <- find_ g dst
  pure $ abs (di - si) + abs (dj - sj)
