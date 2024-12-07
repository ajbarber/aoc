{-# LANGUAGE BlockArguments #-}
module Day5 where

import qualified Data.IntMap as M
import Data.List
import Data.Function
import Control.Monad.State
import Control.Monad (forM_)
import Control.Arrow
import Data.List.Extra (splitOn)

main :: IO ()
main = do
  str <- readFile "Day5.txt"
  let (rules, arrs) = execState (parse str) ([], [])
  let arrs' = reverse arrs
  let res = (\(arr, m) -> if (check rules arr) then m else 0) <$> (\x -> (x, (mid x))) <$> arrs'
  let part1 = sum res
  print part1

parse ::  String -> State ([(Int, Int)], [[Int]]) ()
parse str = let (f, s) = break  (=="") (lines str) in do
  forM_ f \line ->
    let (a, b) = break (=='|') line in
    modify (first ((read a, read (tail b)):))
  forM_ (tail s) \line ->
    modify (second (((read) <$> (splitOn "," line)):))

check :: [(Int, Int)] -> [Int] -> Bool
check rules (x:xs) = all (flip elem rules) (tuples x xs) && check rules xs
check _ [] = True

mid xs = let i = (length xs -1) ` div` 2 in xs !! i

tuples x (y:ys) = (x,y):(tuples x ys)
tuples x [] = []
