module Day1 where

import Prelude
import Data.List

main :: IO ()
main = do
  str <- readFile "Day1.txt"
  let tpls = parse $ lines str
  let arr1 = sort $ fst <$> tpls
  let arr2 = sort $ snd <$> tpls
  let part1 = sum $ abs <$> zipWith (-) arr1 arr2
  let part2 = sum $ zipWith (*) (fst <$> tpls) $ countOccurs (snd <$> tpls) <$> (fst <$> tpls)
  print part1
  print part2

parse :: [String] -> [(Int, Int)]
parse (str:rest) = (read a,read b): parse rest
   where
     a:b:_ = words str
parse [] = []

countOccurs :: [Int] -> Int -> Int
countOccurs xs x = foldr (\a b -> if a == x then b + 1 else b) 0 xs
