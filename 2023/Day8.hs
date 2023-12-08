{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import Prelude
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Data.List.Extra
import Data.Traversable
import Debug.Trace
import Control.Monad.Loops
import Data.Maybe

data Network = Network { instructions :: String,
                         nodes :: M.Map (String, Char) String,
                         currentNodes :: [String],
                         multiples :: [Int]
                         } deriving Show

main :: IO ()
main = do
  str <- readFile "Day8.txt"
  let init = execState (traverse parseLine (lines str)) $ Network { instructions = "", nodes = M.empty, currentNodes = [], multiples=[] }
  let part1 = evalState walk init
  let state2 = execState (walkMultiple 4) init
  let part2 = foldr lcm 1 state2.multiples
  print part1
  print part2

parseLine :: String -> State Network ()
parseLine str = do
  s <- get
  case breakOn "=" str of
    ("", "") -> pure ()
    (a,"") -> put s{instructions = a}
    (a,rest) -> let (l,r) = toTuple (drop 2 rest) in
      put s{ nodes = M.insert (trim a, 'R') r (M.insert (trim a, 'L') l s.nodes)}

toTuple :: String -> (String, String)
toTuple ('(':x:y:z:',':' ':a:b:c:_) = ([x,y,z], [a,b,c])

walk :: State Network (Maybe (Int, Char))
walk = do
  s <- get
  put s { currentNodes = ["AAA"] }
  firstM (\a -> do
     s' <- get
     let xs = (\y -> M.lookup (y, snd a) s'.nodes) <$> s'.currentNodes
     put s'{currentNodes = catMaybes xs}
     pure (Just "ZZZ" `elem` xs)) (zip [1..] $ cycle s.instructions)

-- Run with n > 1 first to verify they are cycling. Then just apply LCM.
walkMultiple :: Int -> State Network (Maybe (Int, Char))
walkMultiple n = do
  s <- get
  let init = filter ((=='A') . last) (everySecond $ fst <$> M.keys s.nodes)
  put $ trace (show init) s{currentNodes = init}
  -- Keep doing the calc until all routes have hit Z.
  firstM (\a -> do
     s' <- get
     let xs' = mapMaybe (\y -> M.lookup (y, snd a) s'.nodes) s'.currentNodes
     let multiples' = s'.multiples <> (fst a <$ filter ((=='Z') . last) xs')
     put s'{currentNodes = xs', multiples = multiples'}
     pure (length multiples' == n * length xs')) (zip [1..] $ cycle s.instructions)

everySecond :: [a] -> [a]
everySecond (a:b:xs) = a:everySecond xs
everySecond [a] = [a]
everySecond [] = []
