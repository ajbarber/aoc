module Day6 where

import Prelude
import Data.Foldable

main :: IO ()
main = do
  str <- readFile "Day6.txt"
  let games = parseGame str
  let distances = product $ zipWith (\ n arr -> length $ filter (> n) arr) (snd <$> games) (map (distance . fst) games)
  print distances

parseGame :: String -> [(Int, Int)]
parseGame str = let (l1:l2:_) = lines str in
  zip (toInts l1) (toInts l2)
  where
    toInts s = read <$> drop 1 (words s)

distance :: Int -> [Int]
distance time = foldl' (\b a -> pressCalc time a:b) [] [0..time]

pressCalc :: Int -> Int -> Int
pressCalc raceTime pressTime = let movementTime = raceTime - pressTime in
  movementTime * pressTime
