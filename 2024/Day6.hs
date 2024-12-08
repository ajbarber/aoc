{-# LANGUAGE BlockArguments #-}
module Day6 where

import qualified Data.Map.Strict as M
import Data.List
import qualified Data.Set as S
import Data.Function
import Control.Monad.State.Strict
import Control.Monad (forM_)
import Data.List.Extra (splitOn)
import Debug.Trace (traceM, trace)

type Grid = M.Map (Int, Int) Char

(|.|)= liftA2 (||)

main :: IO ()
main = do
  str <- readFile "Day6.txt"
  let st = execState (parse str) M.empty
      start = head $ M.keys $ M.filter ((=='^') |.| (=='>') |.| (=='<') |.| (=='v')) st
  print start
  let part1 = execState (step start st) S.empty
  print ("Part 1:" <> show (length $ S.elems part1))

parse ::  String -> State (M.Map (Int,Int) Char) ()
parse str = do
  forMi_ (lines str) \(i, line) -> do
    forMi_ line \(j, c) -> do
      modify (M.insert (i,j) c)

forMi_ :: (Monad m, Enum a0, Num a0) => [b1] -> ((a0, b1) -> m b) -> m ()
forMi_ xs = forM_ (zip [0..] xs)

step :: (Int, Int) -> Grid -> State (S.Set (Int, Int)) Grid
step (m,n) grid = do
  modify (S.insert (m,n))
  case M.lookup (m,n) grid of
      Just '^' -> let move = M.lookup (m-1, n) grid in
        if move == Just '.' then let
         moved = M.update (const $ Just '^') (m-1,n) grid
         deleted = M.update (const $ Just '.') (m, n) moved in
         step (m-1, n) deleted
        else if move == Just '#' then let
         turned = M.update (const $ Just '>') (m,n) grid
         in step (m, n) turned
        else pure grid
      Just '>' -> let move = M.lookup (m, n+1) grid in
        if move == Just '.' then let
          moved = M.update (const $ Just '>') (m,n+1) grid
          deleted = M.update (const $ Just '.') (m, n) moved in
          step (m, n+1) deleted
        else if move == Just '#' then let
          turned = M.update (const $ Just 'v') (m,n) grid
          in step (m, n) turned
        else pure grid
      Just 'v' -> let move = M.lookup (m+1, n) grid in
        if move == Just '.' then let
          moved = M.update (const $ Just 'v') (m+1,n) grid
          deleted = M.update (const $ Just '.') (m, n) moved in
          step (m+1, n) deleted
        else if move == Just '#' then let
          turned = M.update (const $ Just '<') (m,n) grid
          in step (m, n) turned
        else pure grid
      Just '<' -> let move = M.lookup (m, n-1) grid in
        if move == Just '.' then let
          moved = M.update (const $ Just '<') (m,n-1) grid
          deleted = M.update (const $ Just '.') (m, n) moved in
          step (m, n-1) deleted
        else if move == Just '#' then let
          turned = M.update (const $ Just '^') (m,n) grid
          in step (m, n) turned
        else pure grid
      Nothing -> pure grid
