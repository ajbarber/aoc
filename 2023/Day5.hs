{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Day5 where

import qualified Data.Map as M
import qualified Data.IntMap as I
import Data.Traversable
import Data.Maybe
import Control.Monad.State.Strict
import Data.List.Extra
import Debug.Trace
import Data.Ix

data GenericMap = LowHighDelta Int Int Int deriving Show

data SeedState = SeedState { seeds :: [Int],
                             currentCat :: String,
                             categoryMap :: M.Map String [GenericMap] } deriving Show
main :: IO ()
main = do
  str <- readFile "Day5.txt"
  let state' = execState (parse str) $ SeedState { seeds=[], currentCat="", categoryMap=M.empty}
  let part1 = evalState locations state'
  print ("Part 1: " <> show (minimum part1))
  let part2 = evalState locationRanges state'
  print ("Part 1: " <> show (minimum part2))

expandedMap :: [GenericMap] -> String -> String -> String -> [GenericMap]
expandedMap b' x y z = let z' = read z
                           y' = read y
                           x' = read x in
                       LowHighDelta y' (y'+z'-1) (x'-y') :b'

parse :: String -> State SeedState ()
parse str = forM_ (lines str) \l -> do
  st <- get
  case words l of
    ("seeds:": seeds) -> put st { seeds = read . trim <$> seeds }
    [name, "map:"] -> put st { currentCat = name }
    (x: y: z:_) -> put st { categoryMap = M.insert st.currentCat (expandedMap (fromMaybe [] $ M.lookup st.currentCat st.categoryMap) x y z ) st.categoryMap }
    _ -> pure ()

locations :: State SeedState [Int]
locations = do
  st <- get
  mapM chain st.seeds

locationRanges :: State SeedState [Int]
locationRanges = do
  st <- get --transform new seeds
  let newSeeds = concatMap (\(a:b:_) -> (+a) <$> range (0,b-1)) $ chunksOf 2 st.seeds
  mapM chain newSeeds

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
