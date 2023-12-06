module Day6 where

import Prelude
import Data.Foldable
import Data.List.Extra

main :: IO ()
main = do
  str <- readFile "Day6.txt"
  let games = parseGame str
  let distances = product $ zipWith (\ n arr -> length $ filter (> n) arr) (snd <$> games) (map (distance . fst) games)
  print distances
  let (gameTime, record) = parseGamePart2 str
  let (fst,lst) = solution gameTime record
  let ways = 2 + ceiling lst - floor fst
  print ways

parseGame :: String -> [(Int, Int)]
parseGame str = let (l1:l2:_) = lines str in
  zip (toInts l1) (toInts l2)
  where
    toInts s = read <$> drop 1 (words s)

parseGamePart2 :: String -> (Int, Int)
parseGamePart2 str = let (l1:l2:_) = lines str in
  (toInts l1,toInts l2)
  where
    toInts s = read $ filter (/= ' ') $ unwords $ drop 1 (words s)

distance :: Int -> [Int]
distance time = foldl' (\b a -> pressCalc time a:b) [] [0..time]

pressCalc :: Int -> Int -> Int
pressCalc raceTime pressTime = let movementTime = raceTime - pressTime in
  movementTime * pressTime

solution :: Int -> Int -> (Float, Float)
solution gameTime record = let a = -1
                               b = fromIntegral gameTime
                               c = - fromIntegral record in
                             ((-b + sqrt (b^2 - 4*a*c))/(2*a), (-b - sqrt (b^2 - 4*a*c))/(2*a))
