{-# LANGUAGE BlockArguments #-}
module Day14 where

import Prelude
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Loops

main :: IO ()
main = do
  str <- readFile "Day14.txt"
  let parsed = foldr parseLine M.empty (zip [0..] (lines str))
  print $ M.keys parsed
  let part1 = load $ execState stepAll parsed
  print part1

data RockType = O | Cube | Empty deriving (Eq, Show)

type Rocks = M.Map (Int,Int) RockType

toRock 'O' = O
toRock '#' = Cube
toRock _ = Empty

parseLine :: (Int,String) -> Rocks -> Rocks
parseLine (i,str) rocks = foldr (\(j,a) -> M.insert (i,j) (toRock a)) rocks (zip [0..] str)

load :: Rocks -> Int
load r = sum $ map (1+ xMax -) (fst <$> M.keys (M.filter (==O) r))
  where
    ((xMax,_), _) = M.findMax r

moveIfCan :: (Int, Int) -> Rocks -> (Rocks, Bool)
moveIfCan (i,j) r
  | original == Just O = case M.lookup (i-1,j) r of
     Just Empty -> (M.update (const original) (i-1,j) deleted, True)
     _ -> (r, False)
  | otherwise = (r, False)
  where
    original = M.lookup (i,j) r
    deleted = M.update (const (Just Empty)) (i,j) r

-- Final boolean state indicates finished stepping
stepAll :: State Rocks [Bool]
stepAll = iterateUntil and (do
  s <- get
  mapM step (M.keys s))

-- Return value Bool indicates True = no move possible on this step
step :: (Int, Int) -> State Rocks Bool
step (i,j) = do
  s <- get
  let (s', res) = moveIfCan (i,j) s
  not res <$ put s'
