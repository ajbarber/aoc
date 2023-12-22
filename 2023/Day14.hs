{-# LANGUAGE BlockArguments #-}
module Day14 where

import Prelude
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Loops
import Data.List
import Debug.Trace
import Data.Maybe

main :: IO ()
main = do
  str <- readFile "Day14.txt"
  let parsed = foldr parseLine M.empty (zip [0..] (lines str))
  print $ M.keys parsed
  let part1 = load $ execState (stepAll north) parsed
  print part1
  let period= sum $ length <$> evalState findCycleM parsed
  print period
  let part2 = load $ execState (runCycle (1000000000 `mod` period)) parsed
  print part2

data RockType = O | Cube | Empty deriving (Eq, Show)

type Rocks = M.Map (Int,Int) RockType
type Direction = (Int, Int) -> (Int, Int)

toRock 'O' = O
toRock '#' = Cube
toRock _ = Empty

parseLine :: (Int,String) -> Rocks -> Rocks
parseLine (i,str) rocks = foldr (\(j,a) -> M.insert (i,j) (toRock a)) rocks (zip [0..] str)

load :: Rocks -> Int
load r = sum $ map (1+ xMax -) (fst <$> M.keys (M.filter (==O) r))
  where
    ((xMax,_), _) = M.findMax r

north (a,b) = (a-1, b)
south (a,b) = (a+1, b)
west (a,b) = (a, b-1)
east (a,b) = (a, b+1)

runCycle :: Int -> State Rocks [Int]
runCycle n = forM (take (n*4) $ cycle [north, west, south, east]) \d -> do
  stepAll d
  gets load

moveIfCan :: (Int, Int) -> Direction -> Rocks -> (Rocks, Bool)
moveIfCan a dir r
  | original == Just O = case M.lookup (dir a) r of
     Just Empty -> (M.update (const original) (dir a) deleted, True)
     _ -> (r, False)
  | otherwise = (r, False)
  where
    original = M.lookup a r
    deleted = M.update (const (Just Empty)) a r

-- Final boolean state indicates finished stepping
stepAll :: Direction -> State Rocks [Bool]
stepAll dir = iterateUntil and (do
  s <- get
  mapM (step dir) (sortedKeys dir (M.keys s)))

sortedKeys :: Direction -> [(Int,Int)] -> [(Int, Int)]
sortedKeys dir ks = case dir (0,0) of
  (-1,0) -> sort ks                  -- N [(1,2), (1,3), (3,4), (3,5)]
  (1,0) -> sortBy (flip compare) ks  -- S [(3,5),(3,4),(1,3),(1,2)]
  (0,-1) -> sort ks                  -- W [(1,2), (1,3), (3,4), (3,5)]
  (0,1) -> sortBy (flip compare) ks  -- E [(3,5),(3,4),(1,3),(1,2)]

-- Return value Bool indicates True = no move possible on this step
step :: Direction-> (Int, Int) -> State Rocks Bool
step dir a = do
  s <- get
  let (s', res) = moveIfCan a dir s
  not res <$ put s'

-- Non monadic very crude version of cycle detection
findCycle :: [Int] -> Maybe Int
findCycle xs = (2+) <$> findIndex (\arr -> sum (take window $ drop startBuf arr) == 0) diffs
     where
       startBuf = 20
       window = 100
       diffs = map (\i -> zipWith (-) xs (drop i xs)) [2..1000]

-- Cycle detection in State, advance forward 100 cycles
-- to allow the series to settle down
-- then start detecting from that point
findCycleM :: State Rocks [[Int]]
findCycleM = do
  runCycle 100
  s <- get
  moveUntilCycle s

-- Keeps a record of all the loads of every run and cycle within that run
moveUntilCycle :: Rocks -> State Rocks [[Int]]
moveUntilCycle orig = runCycle 1 `untilM` (do
  s <- get
  pure $ trace ("Checking load" <> show (load s) <> show (load orig)) load s == load orig)
