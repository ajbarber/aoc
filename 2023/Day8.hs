{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Day8 where

import Prelude
import qualified Data.Map as M
import Control.Monad.State
import Data.List.Extra
import Data.Traversable
import Debug.Trace
import Control.Monad.Loops
import Data.Maybe

data Network = Network { instructions :: String, nodes :: M.Map (String,Char) String, currentNode :: String } deriving Show

main :: IO ()
main = do
  str <- readFile "Day8.txt"
  let parsed = execState (traverse parseLine (lines str)) $ Network { instructions = "", nodes = M.empty, currentNode = "AAA" }
  --print parsed
  --print parsed
  ---print  $ M.lookup ("AAA", 'R') parsed.nodes
  let part1 = evalState walk parsed
  print part1

parseLine :: String -> State Network ()
parseLine str = do
  s <- get
  case breakOn "=" str of
    ("", "") -> pure ()
    (a,"") -> put s{instructions = a}
    (a,rest) -> let (l,r) = toTuple (drop 2 rest) in put s{ nodes = M.insert (trim a, 'R') r (M.insert (trim a, 'L') l s.nodes)}

toTuple :: String -> (String, String)
toTuple ('(':x:y:z:',':' ':a:b:c:_) = ([x,y,z], [a,b,c])

walk :: State Network (Maybe (Int, Char))
walk = do
  s <- get
  firstM (\a -> do
     s' <- get
     traceM s'.currentNode
     let x = M.lookup (s'.currentNode, snd a) s'.nodes
     put s'{currentNode = fromMaybe "" x}
     pure (x==Just "ZZZ")) (zip [1..] $ cycle s.instructions)
