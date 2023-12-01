module Day1 where

import Prelude
import Control.Monad.State
import Control.Monad
import Data.Char
import Data.List.Extra
import qualified Data.Map as M
import Control.Applicative (liftA2)

main :: IO ()
main = do
  str <- readFile "Day1.txt"
  let part1 = foldr ((+) . read . liftA2 (<>) firstInt lastInt) 0 (lines str)
  print part1
  let part2 = foldr ((+) . read . liftA2 (<>) (firstInt . firstWord) (lastInt . lastWord)) 0 ( lines str)
  print part2


lastInt = firstInt . reverse

firstInt :: String -> String
firstInt = take 1 . dropWhile (not . isDigit)

wordMap :: M.Map String String
wordMap = M.fromList [("one", "1"),
                      ("two", "2"),
                      ("three", "3"),
                      ("four", "4"),
                      ("five", "5"),
                      ("six", "6"),
                      ("seven", "7"),
                      ("eight", "8"),
                      ("nine", "9")]

word :: (Int -> Int -> Bool) -> Int -> String -> String
word fn l str = let (_, (w,r)) = execState (replacement fn str) (l, (str,str)) in
  replace w r str

firstWord str = word (<) (length str) str
lastWord = word (>=) 0

replacement :: (Int -> Int -> Bool) -> String -> State (Int, (String, String)) ()
replacement fn str = forM_ (M.toList wordMap) (\(w,r) ->
  forM_ (zip [0..] str) (\(i,s) -> do
    (i',_) <- get
    when (w `isPrefixOf` drop i str && fn i i') $ put (i,(w,r))
  ))
