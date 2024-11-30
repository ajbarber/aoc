module Day1 where

import Prelude

main :: IO ()
main = do
  str <- readFile "Day1.txt"
  print str
