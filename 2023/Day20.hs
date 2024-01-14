{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
module Day20 where

import Prelude

import Control.Monad
import Data.Bifunctor
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Extra
import Data.Foldable
import Debug.Trace
import Control.Monad.State.Strict
import Data.Maybe

data NodeType = Conjunction | Flip | Broadcast | Unknown deriving (Show,Eq)

data Node = Node { t :: NodeType, on :: Bool, inputs :: [String], outputs :: [String]  } deriving (Show)

-- Signal type True = High
type Connections = M.Map (String, String) Bool

type NodeState = (M.Map String Node, Connections)

main :: IO ()
main = do
  str <- readFile "Day20.txt"
  let st = foldl' parseLine M.empty (lines str)
  let (a,s) = runState runs (st, M.empty)
  let highlows = concat a
  print (length (filter id highlows) * length (filter not highlows))

runs = replicateM 1000 bfs

-- Parse into node and neighbours
-- broadcaster -> a, b, c
parseLine :: M.Map String Node -> String -> M.Map String Node
parseLine st line = let (lhs:rhs:_) = splitOn "->" line
                        (typ,lhs_) = toType (trim lhs)
                        rhs_ = trimStart <$> splitOn "," rhs
                        m = M.alter (\case
                                      Just a' -> Just (a' { t = typ,
                                                            on = False,
                                                            outputs = rhs_ })
                                      Nothing -> Just (Node { t = typ,
                                                              on = False,
                                                              inputs = [],
                                                              outputs = rhs_})) lhs_ st
                        m' = foldl' (flip (M.alter (\case
                                      Just a' -> Just (a' { inputs = lhs_:a'.inputs })
                                      Nothing -> Just (Node { t = Unknown,
                                                              on = False,
                                                              inputs = [lhs_],
                                                              outputs= []})))) m rhs_ in m'

toType :: String -> (NodeType, String)
toType "broadcaster" = (Broadcast, "broadcaster")
toType ('&':xs) = (Conjunction, xs)
toType ('%':xs) = (Flip, xs)

-- broadcaster -> a, b, c
-- %a -> b
-- %b -> c
-- %c -> inv
-- &inv -> a

-- BFS on Nodes
-- as we visit each node;
-- if flip flop:
-- high pulse -> do nothing
-- low pulse -> if off turn on and send high pulse
--           -> if on turns off and sends a low pulse
-- if conjunction:
-- 1. update memory for that input - high or low
-- 2. if it remembers all high pulses for connected inputs -> send low pulse
--    otherwise send high pulse

bfs :: State NodeState [Bool]
bfs = go [(False, "", "broadcaster")] []
  where
    go :: [(Bool, String,String)] -> [Bool] -> State NodeState [Bool]
    go [] acc = pure acc
    go ((sig, i,c):queue) acc = do
      (ns, connections) <- get
      let cur = ns M.! c
          input = ns M.! i
          neighbours s = queue <> ((s,c,) <$> cur.outputs)
          connected = (\k -> M.lookup (k,c) connections) <$> cur.inputs
      modify (second (M.insert (i, c) sig))
      if cur.t == Flip && sig then go queue (sig:acc) --ignore high signal
      else if cur.t == Flip && cur.on then do
         modify (first (M.update (Just . \r -> r {on = False}) c)) -- turn off
         go (neighbours False) (sig:acc)
      else if cur.t == Flip && not cur.on then do
         modify (first (M.update (Just . \r -> r {on = True}) c)) -- turn on
         go (neighbours True) (sig:acc)
      else if cur.t == Conjunction then do
         (_,connections') <- get
         let connected = (\k -> M.lookup (k,c) connections') <$> cur.inputs
             allHigh = all (== Just True) connected in
             go (neighbours (not allHigh)) (sig:acc)
      else do
         go (neighbours sig) (sig:acc)
