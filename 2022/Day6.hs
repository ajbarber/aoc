module Day6 where

import Prelude
import Control.Monad.State
import Control.Monad.Extra
import Data.List (sort)
import Debug.Trace

main :: IO ()
main = do
  str <- readFile "2022/Day6.txt"
  let (_, part1) = execState (parseSignal 4 str) (0,0)
  let (_, part2) = execState (parseSignal 14 str) (0,0)
  print  $ "Part 1 " <> show (part1 + 1)
  print $ "Part 2 " <> show (part2 + 1)

parseSignal :: Int -> String -> State (Int,Int) ()
parseSignal len str = whileM (process len str)

process :: Int -> String -> State (Int, Int) Bool
process i str = do
  (start, end) <- get
  if (end - start) < i-1 then
    True <$ put (start, end + 1)
  else if unique (start, end) str then
    pure False
  else
    True <$ put (start+1, end+1)

unique :: (Int, Int) -> String -> Bool
unique (start, end) = isUniq . substring (start, end)

substring :: (Int, Int) -> String -> String
substring (i,j) = take (j - i + 1) . drop i

isUniq :: String -> Bool
isUniq as = go Nothing (sort as)
   where
     go Nothing (a:xs) = go (Just a) xs
     go (Just p) (a:xs) = p /= a && go (Just a) xs
     go _ [] = True
