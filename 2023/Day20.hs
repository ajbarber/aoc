{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
module Day20 where

import Prelude

import Control.Monad
import Data.Bifunctor
import qualified Data.Map as M
import Data.List.Extra
import Data.Either
import Control.Monad.State.Strict
import Data.Maybe
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Extra

data NodeType = Conjunction | Flip | Broadcast | Unknown deriving (Show,Eq)

data Node = Node { t :: NodeType, on :: Bool, inputs :: [String], outputs :: [String]  } deriving (Show)

-- Signal type True = High
type Connections = M.Map (String, String) Bool

type NodeState = (M.Map String Node, Connections)

data OnOff = On | Off deriving (Eq)

main :: IO ()
main = do
  str <- readFile "Day20.txt"
  let st = foldl' parseLine M.empty (lines str)
  let highlows = evalState (execWriterT part1) (st, M.empty)
  print ("Part 1 " <> show (length (filter id highlows) * length (filter not highlows)))
  let rxInputs = parents st "rx"
  let periods = (\m -> evalState (part2 m) (st, M.empty)) <$> rxInputs
  print ("Part 2 " <> show (foldr lcm 1 periods))

part1 :: WriterT [Bool] (State NodeState) [Bool]
part1 = replicateM 1000 (bfs Nothing)

part2 :: String -> State NodeState Integer
part2 mod = loopM (\i -> do
                  (a,w) <- runWriterT (bfs (Just mod))
                  pure $ if a then Right i
                         else Left (i+1)) 1

parents :: M.Map String Node -> String -> [String]
parents m str = let node = m M.! str in
     if length node.inputs > 1 then node.inputs
     else parents m (head node.inputs)

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

-- Takes an optional argument specifying a module to
-- test for a low signal output, and returns early if detected.
-- Used in part2 for the cycle detection
bfs :: Maybe String -> WriterT [Bool] (State NodeState) Bool
bfs mod = go [(False, "", "broadcaster")]
  where
    go [] = return False
    go ((sig, i,c):queue) = do
      (ns, connections) <- lift get
      let cur = ns M.! c
          input = ns M.! i
          neighbours s = queue <> ((s,c,) <$> cur.outputs)
          connected = (\k -> M.lookup (k,c) connections) <$> cur.inputs
      lift $  modify (second (M.insert (i, c) sig))
      tell [sig]
      if isJust mod && c == fromJust mod && not sig then pure True
      else if cur.t == Flip && sig then go queue  --ignore high signal
      else if cur.t == Flip && cur.on then do
         switch c Off
         go (neighbours False)
      else if cur.t == Flip && not cur.on then do
         switch c On
         go (neighbours True)
      else if cur.t == Conjunction then do
         (_,connections') <- lift get
         let connected = (\k -> M.lookup (k,c) connections') <$> cur.inputs
             allHigh = all (== Just True) connected in
             go (neighbours (not allHigh))
      else go (neighbours sig)

switch :: String -> OnOff -> WriterT [Bool] (State NodeState) ()
switch c on = lift $ modify (first (M.update (Just . \r -> r {on = on == On}) c))
