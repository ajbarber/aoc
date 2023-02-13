{-# LANGUAGE DeriveFunctor #-}
module Day13 where

import Prelude
import Data.Char
import Data.Tuple
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace
import Test.QuickCheck

import Data.Either

data Node a = Leaf a | List [Node a] deriving (Show, Functor, Read, Eq)

comp :: Node Int -> Node Int -> Either Bool Bool
comp (List (x:xs)) (List (y:ys)) = comp x y <> comp (List xs) (List ys)
comp (Leaf x) (Leaf y)
  | x < y = Right True -- in order
  | x == y =  Left False -- keep going
  | otherwise = Right False  -- not in order
comp (Leaf x) (List n) = comp (List [Leaf x]) (List n)
comp (List n) (Leaf x) = comp (List n) (List [Leaf x])
comp (List []) (List []) = Left False -- keep going, no decision yet!
comp (List []) (List _) = Right True
comp (List x) (List []) = Right False

order :: Node Int -> Node Int -> Ordering
order n1 n2 = case comp n1 n2 of
  Right True -> LT
  Right False -> GT
  Left _ -> GT

-- Parsing can be done in one pass
-- Idea is push each new [] onto a stack.
-- then for each ] pop off 2nd and 1st and nest 2nd [ 1st] as the new head, then recurse
-- See example stack trace

-- [1,[2,[3,[4,[5,6,7]]]],8,9]
-- [1,[2,[3,[4,[5,6,0]]]],8,9]

-- parse "[1,[2,[3,[4,[5,6,7]]]],8,9]" []
-- parse "1,[2,[3,[4,[5,6,7]]]],8,9]"  [List[]]
-- parse ",[2,[3,[4,[5,6,7]]]],8,9]"  [List[1]]
-- parse "2,[3,[4,[5,6,7]]]],8,9]"  [List [], List[1] ]
-- parse "[3,[4,[5,6,7]]]],8,9]"  [List [2], List[1]]
-- parse "3,[4,[5,6,7]]]],8,9]"  [List [], List [2], List[1]]
-- parse ",[4,[5,6,7]]]],8,9]"  [List [3], List [2], List[1]]
-- parse "[4,[5,6,7]]]],8,9]"  [List [], List [3], List [2], List[1]]
-- parse "5,6,7]]]],8,9]"  [List [], List [4], List [3], List [2], List[1]]
-- parse "6,7]]]],8,9]"  [List [5], List [4], List [3], List [2], List[1]]
-- parse "7]]]],8,9]"  [List [5,6], List [4], List [3], List [2], List[1]]
-- parse "]]]],8,9]"  [List [5,6,7], List [4], List [3], List [2], List[1]]
-- parse "]]],8,9]"  [List [4, List [5,6,7]], List [3], List [2], List[1]]
-- parse "]],8,9]"  [List [3, List [4, List [5,6,7]]], List [2], List[1]]
-- parse "],8,9]"  [List [2, List [3, List [4, List [5,6,7]]]], List[1]]
-- parse ",8,9]"  [List [1, List [2, List [3, List [4, List [5,6,7]]]]]]
-- parse ",8,9]"  [List [1, List [2, List [3, List [4, List [5,6,7]]]]]]
-- parse "]"  [List [1, List [2, List [3, List [4, List [5,6,7]]]],Leaf 8, Leaf 9]]
-- = List [1, List [2, List [3, List [4, List [5,6,7]]]],Leaf 8, Leaf 9]

parse :: String -> [Node Int] -> Node Int
parse ('[':xs) a = parse xs (List [] : a)
parse (']':xs) (r1 : List r2 : rest) = parse xs (List (r2 ++ [r1]):rest)
parse "]" res = head res
parse (',':xs) a = parse xs a
parse ('1':'0':xs) ((List a) : as) = parse xs (List (a ++ [Leaf 10]) : as)
parse (x:xs) ((List a) : as) = parse xs (List (a ++ [Leaf $ digitToInt x]) : as)
parse [] res = head res

-- Interesting aside: you can cobble together string
-- literals of ADTs and use deriving Read
x :: Node Int
x = List [read "List [Leaf 1,Leaf 2,Leaf 3]"]

main :: IO ()
main = do
  chunks <- splitOn "\n\n" <$> readFile "2022/Day13.txt"
  let part1 = sum . fmap fst . filter snd . zip [1..] $ doCmp . lines <$> chunks
  let xs = sortBy order $ (flip parse [] <$> concatMap lines chunks) <> dividers
  let part2 = (*) <$> ((1+) <$> elemIndex d1 xs) <*> ((1+) <$> elemIndex d2 xs)
  putStrLn ("Part 1:"<> show part1)
  putStrLn ("Part 2:"<> show part2)

-- | The two dividers supplied
d1 = parse "[[2]]" []
d2 = parse "[[6]]" []

dividers :: [Node Int]
dividers = [ d1, d2 ]

doCmp :: [String] -> Bool
doCmp (x:y:_) = comp (parse x []) (parse y []) == Right True

-- Testing of given examples

--[1,1,3,1,1]
--[1,1,5,1,1]

x1 = List [Leaf 1, Leaf 1, Leaf 3, Leaf 1, Leaf 1]
y1 = List [Leaf 1, Leaf 1, Leaf 5, Leaf 1, Leaf 1]

-- [[1],[2,3,4]]
-- [[1],4]

x2 = List [ List [ Leaf 1 ], List [ Leaf 2, Leaf 3, Leaf 4]]
y2 = List [ List [ Leaf 1 ], Leaf 4]

-- [9]
-- [[8,7,6]]

x3 = List [Leaf 9]
y3 = List [Leaf 8, Leaf 7, Leaf 6]

-- [[4,4],4,4]
-- [[4,4],4,4,4]

x4 = List [List [Leaf 4, Leaf 4], Leaf 4, Leaf 4]
y4 = List [List [Leaf 4, Leaf 4], Leaf 4, Leaf 4, Leaf 4]

-- [7,7,7,7]
-- [7,7,7]

x5 = List [Leaf 7, Leaf 7, Leaf 7, Leaf 7]
y5 = List [Leaf 7, Leaf 7, Leaf 7]

-- []
-- [3]

x6 = List []
y6 = List [Leaf 3]

-- [[[]]]
-- [[]]

x7 = List [List [ List []]]
y7 = List [List []]

-- [1,[2,[3,[4,[5,6,7]]]],8,9]
-- [1,[2,[3,[4,[5,6,0]]]],8,9]

x8 = List [Leaf 1, List [Leaf 2, List [Leaf 3, List [Leaf 4, List [Leaf 5, Leaf 6, Leaf 7]]]],Leaf 8, Leaf 9]
y8 = List [Leaf 1, List [Leaf 2, List [Leaf 3, List [Leaf 4, List [Leaf 5, Leaf 6, Leaf 0]]]],Leaf 8, Leaf 9]

propSorted1 :: Bool
propSorted1 = comp x1 y1 == Right True

propSorted2 :: Bool
propSorted2 = comp x2 y2 == Right True

propSorted3 :: Bool
propSorted3 = comp x3 y3 == Right False

propSorted4 :: Bool
propSorted4 = comp x4 y4 == Right True

propSorted5 :: Bool
propSorted5 = comp x5 y5 == Right False

propSorted6 :: Bool
propSorted6 = comp x6 y6 == Right True

propSorted7 :: Bool
propSorted7 = comp x7 y7 == Right False
