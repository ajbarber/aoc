module Day7 where

import Prelude hiding (filter)
import Control.Monad (foldM)
import Control.Monad.State
import Data.Map (Map, insert, adjust, empty, filter, foldl)
import Data.List (tails, inits)
import Debug.Trace

-- Commands modelling the interpreter state
data Command = Ls | Cd String | File Int | Dir String | Unknown deriving Show

-- Our filesystem state
data FilesState  = FilesState { file :: Map [String] Int,
                                path :: [String] } deriving Show

main :: IO ()
main = do
  str <- readFile "Day7.txt"
  let res = execState (parse str) init
  let sum = Data.Map.foldl (+) 0 (filter (<=100000) $ file res)
  print sum
  where
    init = FilesState { path = [], file = empty }

parse :: String -> State FilesState ()
parse str = do
  init <- get
  forM_ (lines str) parseLine

parseLine :: String -> State FilesState ()
parseLine s = case command s of
  Cd ".." -> goUp
  Cd dir' -> goDown dir'
  File bytes -> addBytes bytes
  _ -> pure ()

goUp :: State FilesState ()
goUp = do
  s <- get
  put s { path = tail $ path s }

goDown :: String -> State FilesState ()
goDown dir = do
  st <- get
  put $ st { path = dir:path st, file = insert (dir:path st) 0 (file st)  }

addBytes :: Int -> State FilesState ()
addBytes bytes = do
  st <- get
  let key = tails $ path st
  let paths' = take (length key - 1) key
  traceM $ "Here:" <> show st
  forM_ paths' (\k -> do
    st' <- get
    put $ st' { file = adjust (bytes+) k (file st') })

command s = case words s of
  ["$", "ls"] -> Ls
  ["$", "cd", str] -> Cd str
  ["dir", dir] -> Dir dir
  [bytes, filename] -> File (read bytes)
  _ -> Unknown
