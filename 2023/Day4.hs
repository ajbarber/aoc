{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
module Day4 where

import Prelude
import Data.List.Extra
import qualified Data.Set as S
import qualified Data.IntMap as M
import Control.Monad.State
import Data.Maybe

main :: IO ()
main = do
  str <- readFile "Day4.txt"
  let n = length $ lines str
  let part1 = sum $ points . parseLine <$> lines str
  print part1
  let part2 = sum $ M.elems $ execState (game str) $ M.fromList ((,1) <$> [0..n-1])
  print part2

parseLine :: String -> (S.Set Int, [Int])
parseLine l = let (game,wins) = drop 2 <$> breakOn "|" l
                  (_,game') = drop 2 <$> breakOn ":" game in
             (S.fromList (read <$> words game'), read <$> words wins)

points :: (S.Set Int, [Int]) -> Int
points (winning, draws)
   | n == 0 = 0
   | n > 0 = 2 ^ (n-1)
   where
     n = length (filter (`S.member` winning) draws)

game :: String -> State (M.IntMap Int) ()
game str = forM_ (zip [0..] (parseLine <$> lines str)) \(n, (winning,draws)) -> do
  s <- get
  let matches = length $ filter (`S.member` winning) draws
      copies = [n+1..n+matches]
      reps = fromMaybe 1 $ M.lookup n s
      s' = foldl' (flip (M.adjust (+reps))) s copies
  put s'
