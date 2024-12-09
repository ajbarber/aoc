module Day9 where

import Data.List
import Data.Char
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

main :: IO ()
main = do
  str <- readFile "Day9.txt"
  let res = toDot True 0 (read <$> takeWhile (/="\n") ((\x -> [x]) <$> str))
  print $ take 100 res
  let moved = backFill (V.fromList res) (7, length res-1)
  let part1 = V.sum $ V.zipWith (*) (V.fromList [0..]) (V.map read (V.takeWhile (/=".") moved))
  print part1

toDot :: Bool -> Int-> [Int] -> [String]
toDot isId id (x:rest) = first <> (toDot (not isId) (if isId then id+1 else id) rest)
  where first = case isId of
          True -> replicate x (show id)
          False -> replicate x "."
toDot isId id [] = []

backFill :: V.Vector String -> (Int,Int) -> V.Vector String
backFill xs (i,j) = case (i < j) of
  True -> let
    next = V.modify (\v -> do
                      MV.write v j "."
                      MV.write v i (xs V.! j)) xs
    nexti = i + V.length (V.takeWhile (/=".") (V.drop i next))
    nextj = j - 1
    in backFill next (nexti, nextj)
  False -> xs
