module Day1 where

import Prelude
import Control.Monad.State
import Control.Monad
import Data.Char

main :: IO ()
main = do
  str <- readFile "Day1.txt"
  let part1 = foldr ((+) . read . (\x -> firstInt x <> lastInt x)) 0 (lines str)
  print part1

lastInt :: String -> String
lastInt = firstInt . reverse

firstInt :: String -> String
firstInt = take 1 . dropWhile (not . isDigit)
