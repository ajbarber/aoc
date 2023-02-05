module Day13 where

import Prelude
import Test.QuickCheck

data Node a = Leaf a | List [Node a]

comp :: Node Int -> Node Int -> Bool
comp (Leaf x) (Leaf y)
  | x <= y = True
  | otherwise = False
comp (List (x:xs)) (List (y:ys)) =
  comp x y && comp (List xs) (List ys)

main :: IO ()
main = do
  putStrLn "hello"

-- Examples

--[1,1,3,1,1]
--[1,1,5,1,1]

propSorted ::
