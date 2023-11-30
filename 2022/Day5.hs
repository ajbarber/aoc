module Day5 where

import Prelude
import qualified Data.Map as M
import Debug.Trace

type Move = (Int, Int, Int)
type StackState = M.Map Int [String]

main :: IO ()
main = do
  str <- readFile "2022/Day5.txt"
  let (stacks, ms) = parseAll str
  let part m = filter (/=']') . filter (/='[') $ describe $ m stacks ms
  print $ part moves1
  print $ part moves2

parseAll :: String ->  (StackState,  [Move])
parseAll str = (parseStack first, parseMoves second)
   where
     first = takeWhile (\l -> l !! 1 /= '1') (lines str)
     second = tail $ dropWhile (/="") (lines str)

parseStack :: [String] -> StackState
parseStack ls = foldr parseStackLine M.empty (zip [1..] ls)

parseStackLine ::  (Int, String) -> StackState -> StackState
parseStackLine (i,l) state = foldl (\b (i,a) -> M.insertWith (++) i [a] b) state $ filter (("   " /=) . snd) $ zip [1..] (fixedWords l)

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

moveMultiple :: StackState -> (Int, Int, Int) -> StackState
moveMultiple state (n, source, dest) = case M.lookup source state of
  Just xs -> let (f,s) = splitAt n xs in
    M.insertWith (++) dest f (M.insert source s state)
  _ -> state

moveOnce :: (Int, Int) -> StackState -> StackState
moveOnce (source, dest) state = case M.lookup source state of
  Just (x:xs) -> M.insertWith (++) dest [x] (M.insert source xs state)
  Just [] -> state
  Nothing -> trace ("&&&&&" <> show state) state

moves1 :: StackState -> [Move] -> StackState
moves1 = foldl move

moves2 :: StackState -> [Move] -> StackState
moves2 = foldl moveMultiple

describe :: StackState -> String
describe state = trace (show state) foldr ((<>) . safeHead) "" $ M.elems state

safeHead :: [String] -> String
safeHead (x:xs) = x
safeHead [] = ""
