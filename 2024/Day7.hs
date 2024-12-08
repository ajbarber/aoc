{-# LANGUAGE BlockArguments #-}
module Day7 where

import qualified Data.IntMap.Strict as M
import Control.Monad.State
import Control.Monad (forM_)
import Debug.Trace

main :: IO ()
main = do
  str <- readFile "Day7.txt"
  let st = execState (parse str) []
  print st
  print $ length st
  let part1 = sum $ map (\(target, arr) -> if check target arr then target else 0) st
  print part1

parse :: String -> State [(Int,[Int])] ()
parse str = do
  forM_ (lines str) \line -> do
    let (lhs, rhs) = break (==':') line
        rhs' = read <$> words (tail rhs)
    modify (((read lhs), rhs'):)

check :: Int -> [Int] -> Bool
check target (x:xs) = check' target x xs

check' :: Int -> Int -> [Int] -> Bool
check' target accum (x:xs) = check' target (accum + x) xs || check' target (accum * x) xs
check' target accum [] = accum == target
