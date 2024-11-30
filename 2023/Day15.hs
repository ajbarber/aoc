{-# LANGUAGE BlockArguments #-}
module Day15 where

import Prelude
import Data.List.Extra
import Data.Char
import Control.Monad (forM_, liftM2)
import qualified Data.Map as M
import Control.Monad.State.Strict
import Data.Maybe
import Debug.Trace

main :: IO ()
main = do
  str <- readFile "Day15.txt"
  let arr = wordsBy (==',') (filter (/= '\n') str)
  --print arr
  let res = foldl ((. code) . (+)) 0  arr
  --print res
  let part2 = boxSum $ execState (boxes arr) initialBoxes
  print part2

data Lense = Lense String Int deriving (Show, Eq)
data Operation = Delete | Update deriving (Show, Eq)

type BoxMap = M.Map Int [Lense]

code :: String -> Int
code = foldl (\b a -> ((b + ord a) * 17) `mod` 256) 0

{- Part 2
 -- Algo description --
 b <- decode box number
 Parse instruction into lense name, operation, focal length
 Retrieve lenses from box b
 case operation
     "-" -> delete that lens
     "=" -> if (existing lense) replace existing lense with new focal length
            else push lense onto back of lense list
-}

initialBoxes :: BoxMap
initialBoxes = foldr (`M.insert` []) M.empty [0..255]

updateLense :: Lense -> [Lense] -> [Lense]
updateLense l@(Lense name newFl) lenses = if found then replaced else lenses <> [l]
  where
    found = isJust $ find (\(Lense x _) -> x == name) lenses
    replaced = map (\(Lense x y) -> if x == name then Lense name newFl else Lense x y) lenses

deleteLense :: String -> [Lense] -> [Lense]
deleteLense name = filter (\(Lense x _) -> name /= x)

parse :: String -> (String, Operation, Int)
parse str = let tmp = wordsBy (=='=') str in case length tmp of
  2 -> (head tmp, Update, read $ tmp !! 1)
  _ -> (take (length str - 1) str, Delete, -1)

(&&&) = liftM2 (&&)

boxes :: [String] -> State BoxMap ()
boxes xs = forM_ xs (\instr -> do
  s <- get
  let (name, operation, len) = trace instr parse instr
      boxNum = code $ takeWhile ((/='-') &&& (/='=')) instr
      lenses = M.lookup boxNum s
      f = if operation == Delete
          then M.adjust (deleteLense name) boxNum
          else M.adjust (updateLense (Lense name len)) boxNum
  modify f)

focal :: Lense -> Int
focal (Lense a b) = b

boxSum :: BoxMap -> Int
boxSum box = foldr (\(n, arr) b -> b + (1+n) * sum (zipWith (*) (focal <$> arr) [1..])) 0 (M.assocs box)
