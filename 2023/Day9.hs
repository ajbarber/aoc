{-# LANGUAGE BlockArguments #-}
module Day9 where

import Prelude
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Loops
import Debug.Trace
import Data.Foldable
import Data.Maybe

main :: IO ()
main = do
  str <- readFile "Day9.txt"
  let rows = parseLine <$> lines str
  let accums = map (reverse . snd . runWriter . accumDiffs) rows
  let part1 =  sum $ map (last . propagate) accums
  print part1

parseLine :: String -> [Int]
parseLine str = read <$> words str

accumDiffs :: [Int] -> Writer [[Int]] (Maybe [Int])
accumDiffs xs = firstM (\xs' -> do
  tell [xs']
  pure (all (==0) xs')) (iterate diff xs)

diff :: (Num c, Show c) => [c] -> [c]
diff xs = zipWith (-) (tail xs) xs

propagate :: [[Int]] -> [Int]
propagate (x:xs) = foldl' (\b a -> head a:zipWith (+) (b <> [last b]) a) (x<>[0]) xs
