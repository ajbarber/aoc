module Day2 where

import Prelude
import Data.List
import Data.Maybe

(|||) = liftA2 (||)

main :: IO ()
main = do
  str <- readFile "Day2.txt"
  let arr = map read <$> words <$> lines str
  let part1 = results (bothWays <$> arr)
  let part2 = results (part2Ways <$> arr)
  print part1
  print part2

results ::  [Maybe Bool] -> Int
results tpls = length $ filter ((== Just True)) tpls

bothWays :: [Int] -> Maybe Bool
bothWays xs = (|||) (safe xs) (safe (reverse xs))

dropIdx :: [Int] -> Int -> [Int]
dropIdx xs i = snd <$> (filter ((/=i) . fst) $ zip [0..] xs)

permutations' :: [Int] -> [[Int]]
permutations' xs = dropIdx xs <$> [0..length xs-1]

part2Ways :: [Int] -> Maybe Bool
part2Ways xs = foldl1' (|||) (bothWays <$> (permutations' xs))

safe :: [Int] -> Maybe Bool
safe xs = fmap fst $ foldl (\x a -> case x of
                    Nothing -> Just (True, a)
                    Just (asc, last) ->
                      let diff = a - last
                          isAscending = diff <= 3 && diff >= 1 in
                      if isAscending then Just (asc, a)
                      else Just (False, a)) Nothing xs
