{-# LANGUAGE RecordWildCards #-}
module Day9 where

import Prelude
import Linear.V2
import Control.Arrow
import Control.Monad.State
import Data.List
import qualified Data.Set as S
import Data.Foldable (traverse_)
import Debug.Trace

data Direction = U | L | R | D

data RopeState = RopeState { positions :: [V2 Int], visited :: S.Set (V2 Int) }

example :: String
example = "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"

example2 :: String
example2 = "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20"

type Input = [Direction]

main = do
  contents <- readFile "2022/Day9.txt"
  print $ solve $ parse contents

readInt :: String -> Int
readInt = read

parse :: String -> [V2 Int]
parse str = concatMap ((\(dir: count:_) ->
     replicate (read count) $ case dir of
         "R" -> V2   0    1  -- right
         "L" -> V2   0  (-1) -- left
         "D" -> V2   1    0  -- down
         "U" -> V2 (-1)   0) . words) $ lines str  -- up arr

initStatePart1 = RopeState [V2 0 0] $ S.singleton (V2 0 0)

initStatePart2 = RopeState [V2 0 0, V2 0 0, V2 0 0, V2 0 0, V2 0 0, V2 0 0, V2 0 0, V2 0 0, V2 0 0] $ S.singleton (V2 0 0)

solve :: [V2 Int] -> Int
solve moves = let rs = flip execState initStatePart2 $ traverse_ move (scanl (+) (V2 0 0) moves) in length . S.elems $ visited rs

move :: V2 Int -> State RopeState ()
move h = do
  RopeState{..} <- get
  let positions' = tail $ scanl follow h positions
  trace (show positions') put $ RopeState positions' $ S.insert (last positions') visited
  where
    follow :: V2 Int -> V2 Int -> V2 Int
    follow h t | sum ((h - t)^2) < 4 = t
               | otherwise = t + signum(h-t)
