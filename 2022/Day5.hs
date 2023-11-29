module Day5 where

import Prelude
import qualified Data.Map as M
import Control.Monad.Trans.Writer.CPS
import Control.Monad.State
import Control.Monad
import Debug.Trace

type Move = (Int, Int, Int)
type StackState = M.Map Int [String]

main :: IO ()
main = do
  str <- readFile "Day5.txt"
  let (stacks, ms) = parseAll str
  print stacks
  print ms
  let part1 = filter (/=']') . filter (/='[') $ describe $ moves stacks ms
  print part1

parseAll :: String ->  (StackState,  [Move])
parseAll str = (parseStack first, parseMoves second)
   where
     first = takeWhile (\l -> l !! 1 /= '1') (lines str)
     second = tail $ dropWhile (/="") (lines str)

parseStack :: [String] -> StackState
parseStack ls = foldr parseStackLine M.empty (zip [1..] ls)

parseStackLine ::  (Int, String) -> StackState -> StackState
parseStackLine (i,l) state = foldl (\b (i,a) -> M.insertWith (++) i [a] b) state $ filter (\(i,a) -> a /= "   ") $ zip [1..] (fixedWords l)

parseMoves :: [String] -> [Move]
parseMoves = map parseMoveLine

--  ---^[F]^[G]
fixedWords :: String -> [String]
fixedWords = go "" 1
   where
     go buf i (x:xs) | i `mod` 4 == 0 = buf:go "" (i+1) xs
                     | otherwise = go (buf <> [x]) (i+1) xs
     go buf i [] = [buf]

parseMoveLine :: String -> Move
parseMoveLine = go . words
   where
     go ["move", p1, "from", p2, "to", p3] = (read p1, read p2, read p3)

move :: StackState -> (Int, Int, Int) -> StackState
move state (n, source, dest) = iterate (moveOnce (source, dest)) state !! n

moveOnce :: (Int, Int) -> StackState -> StackState
moveOnce (source, dest) state = case M.lookup source state of
  Just (x:xs) -> M.insertWith (++) dest [x] (M.insert source xs state)
  Just [] -> state
  Nothing -> trace ("&&&&&" <> show state) state

moves :: StackState -> [Move] -> StackState
moves = foldl move

describe :: StackState -> String
describe state = trace (show state) foldr ((<>) . safeHead) "" $ M.elems state

safeHead :: [String] -> String
safeHead (x:xs) = x
safeHead [] = ""
