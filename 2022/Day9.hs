import Prelude
import Linear.V2
import Control.Arrow
import Control.Monad.State
import Data.List
import Data.Foldable (traverse_)
import Debug.Trace (trace)

data Direction = U | L | R | D

example :: String
example = "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"

type Input = [Direction]

main = do
  contents <- readFile "day9.txt"
  traverse print $ scanl (+) (V2 0 0) (parse contents)
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

solve :: [V2 Int] -> Int
solve moves = let arr = flip execState [(V2 0 0)] $ traverse_ move (scanl (+) (V2 0 0) moves) in trace (show arr) length $ nub arr

move :: V2 Int -> State [V2 Int] ()
move h = do
  ts <- get
  let tailPos = head ts in modify (follow h tailPos:)
  where
    follow :: V2 Int -> V2 Int -> V2 Int
    follow h t | sum ((h - t)^2) < 4 = t
               | otherwise = t + signum(h-t)
