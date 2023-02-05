{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Day8Tardis where

import Prelude
import Control.Applicative
import Control.Arrow
import Data.Char (digitToInt)
import Data.Function
import Data.Foldable (traverse_)
import Data.Maybe
import Data.List
import Debug.Trace
import Data.Tuple

import Control.Monad.Tardis

main :: IO ()
main = do
  str <- readFile "2022/Day8.txt"
  print $ solve (parse str)

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

example :: String
example = "30373\n25512\n65332\n33549\n35390"

-- need to reverse the forward state array (2nd arg) before comparison
solve :: [[Int]] -> Int
solve grid = orr (horiStates grid) (vertStates grid)

orr :: [[Bool]] -> [[Bool]] -> Int
orr grid1 grid2 = sum $ zipWith zipRow grid1 grid2

zipRow :: [Bool] -> [Bool] -> Int
zipRow row1 row2 = length . filter (==True) $ zipWith (||) row1 row2

horiStates :: Traversable t => [t Int] -> [[Bool]]
horiStates = map rowState

vertStates :: [[Int]] -> [[Bool]]
vertStates = transpose . map rowState . transpose

rowState row = uncurry (zipWith ((||) `on` snd )) $ fmap reverse $ flip execTardis ([], []) $ traverse test row

test :: (MonadTardis [(Int, Bool)] [(Int, Bool)] m) => Int -> m ()
test cur = do
  -- forwards travelling
  past <- getPast
  let pm = safeHead past
  sendFuture ((max (fst pm) cur, cur > fst pm):past) -- first prev is the inital state. cur goes forwards.
  rec
     -- backwards travelling
     sendPast ((max (fst fm) cur, cur > fst fm):fut) -- first next is the next getFuture. cur just goes backwards.
     fut <- getFuture  -- takes the value from the next sendpast
     let fm = safeHead fut
  return ()

safeHead :: (Num a) => [(a, Bool)] -> (a, Bool)
safeHead = fromMaybe (-1, False) . (fmap fst <$> uncons)
