module Day13 where

import Prelude
import Test.QuickCheck

data Node a = Leaf a | List [Node a]

comp :: Node Int -> Node Int -> Maybe Bool
comp (Leaf x) (Leaf y)
  | x < y = Just True -- in order
  | x == y =  Just False
  | otherwise = Nothing
comp (List (x:xs)) (List (y:ys)) = (||) <$> comp x y <*> comp (List xs) (List ys)
comp (List []) (List []) = Just True

main :: IO ()
main = do
  putStrLn "hello"

-- Examples

--[1,1,3,1,1]
--[1,1,5,1,1]

-- reifies as

x = List [Leaf 1, Leaf 1, Leaf 3, Leaf 1, Leaf 1]
y = List [Leaf 1, Leaf 1, Leaf 5, Leaf 1, Leaf 1]

propSorted :: Bool
propSorted = comp x y == Just True
