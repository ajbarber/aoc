module Day2 where

import Prelude
import Data.List.Extra

data GameRun a = GameRun a deriving (Show)

main :: IO ()
main = do
  str <- readFile "Day2.txt"
  let runs = parseLine <$> lines str
  print runs
  let q = (12,13,14)
  let part1 = sum . map fst $ filter (possible q . snd) $ zip [1..] runs
  print part1

possible :: (Int, Int, Int) -> GameRun [(Int, Int, Int)] -> Bool
possible given (GameRun runs) = all (valid given) runs

valid :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
valid (l1, l2, l3) (g1, g2, g3) = g1 <= l1 && g2 <= l2 && g3 <= l3

parseLine :: String -> GameRun [(Int, Int, Int)]
parseLine str =
  let gameStr = drop 2 $ dropWhile (/=':') str
      games = wordsBy (==';') gameStr in GameRun $ parseGame <$> games

--"1 red" (1,0,0) "1 green, 3 blue" (0,1,3)
parseGame :: String -> (Int,Int,Int)
parseGame str = let gs = map (toTuple . words . trim) (wordsBy (==',') str) in
  foldl' parseGameStr (0,0,0) gs

toTuple :: [String] -> (Int,String)
toTuple (a:b:_) = (read a, b)

parseGameStr :: (Int, Int, Int) -> (Int, String) -> (Int, Int, Int)
parseGameStr (r,g,b) (d, "red") = (d, g, b)
parseGameStr (r,g,b) (d, "green") = (r, d, b)
parseGameStr (r,g,b) (d, "blue") = (r, g, d)
