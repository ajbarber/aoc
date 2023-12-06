{-# LANGUAGE OverloadedRecordDot #-}

{-# LANGUAGE BlockArguments #-}

module Day5 where

import qualified Data.Map as M
import qualified Data.IntMap as I
import Data.Traversable
import Data.Maybe
import Control.Monad.State.Lazy
import Data.List.Extra
import Debug.Trace
import Data.Ix
import Control.Monad.Loops

data RangeMap = LowHighDelta Int Int Int deriving Show

data SeedState = SeedState { seeds :: [Int],
                             currentCat :: String,
                             categoryMap :: M.Map String [RangeMap] } deriving Show
main :: IO ()
main = do
  str <- readFile "Day5.txt"
  let state' = execState (parse str) $ SeedState { seeds=[], currentCat="", categoryMap=M.empty}
  let part1 = evalState locations state'
  print ("Part 1: " <> show (minimum part1))
  let coarseList = (10000*) <$>[1..]
  let x = evalState (reverseSearch coarseList) state'
  let x' = fromMaybe 0 x
  let part2 = evalState (reverseSearch [x'-10000..x']) state'
  print part2

expandedMap :: [RangeMap] -> String -> String -> String -> [RangeMap]
expandedMap b' x y z = let z' = read z
                           y' = read y
                           x' = read x in
                       LowHighDelta y' (y'+z'-1) (x'-y') :b'

parse :: String -> State SeedState ()
parse str = forM_ (lines str) (\l -> do
  st <- get
  case words l of
    ("seeds:": seeds) -> put st { seeds = read . trim <$> seeds }
    [name, "map:"] -> put st { currentCat = name }
    (x: y: z:_) -> put st { categoryMap = M.insert st.currentCat (expandedMap (fromMaybe [] $ M.lookup st.currentCat st.categoryMap) x y z ) st.categoryMap }
    _ -> pure ())

reverseMaps :: State SeedState()
reverseMaps = do
  st <- get
  put st { categoryMap = M.map (reverseMap <$>) st.categoryMap }

reverseMap :: RangeMap -> RangeMap
reverseMap (LowHighDelta l h d) = LowHighDelta (l + d) (h + d) (-d)

locations :: State SeedState [Int]
locations = do
  st <- get
  mapM chain st.seeds

lookup' :: String -> Int -> State SeedState Int
lookup' mapName key = do
  st <- get
  let gm = fromMaybe [] (M.lookup mapName st.categoryMap)
  let found = find (\(LowHighDelta l h d) -> key <= h && key >= l) gm
  pure $ case found of
    Just (LowHighDelta l h d) -> key + d
    _ -> key

chain :: Int -> State SeedState Int
chain seed = do
  st <- get
  soil <- lookup' "seed-to-soil" seed
  fert <- lookup' "soil-to-fertilizer" soil
  water <- lookup' "fertilizer-to-water" fert
  light <- lookup' "water-to-light" water
  temp <- lookup' "light-to-temperature" light
  humi <- lookup' "temperature-to-humidity" temp
  lookup' "humidity-to-location" humi

reverseChain :: Int -> State SeedState Int
reverseChain loc = do
  st <- get
  humi <- lookup' "humidity-to-location" loc
  temp <- lookup' "temperature-to-humidity" humi
  light <- lookup' "light-to-temperature" temp
  water <- lookup' "water-to-light" light
  fert <- lookup' "fertilizer-to-water" water
  soil <- lookup' "soil-to-fertilizer" fert
  lookup' "seed-to-soil" soil

reverseSearch :: [Int] -> State SeedState (Maybe Int)
reverseSearch space = do
  reverseMaps
  st <- get
  let seedRanges = map (\(a:b:_) -> (a, a+b-1)) $ chunksOf 2 st.seeds
  firstM (\l -> do
    v <- reverseChain l
    traceM ("Checking" <> show v <> "is in range" <> show seedRanges)
    pure (v `inRange'` seedRanges)) space

inRange' :: Int -> [(Int, Int)] -> Bool
inRange' i = foldr (\(l,h) b -> (i <= h && i >= l) || b) False
