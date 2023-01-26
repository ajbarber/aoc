{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
import Prelude
import Control.Monad.Loops
import Control.Monad.State
import Data.Char
import Data.List
import Data.Maybe
import Data.Set qualified as S
import Debug.Trace

import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "Running part 1..."
  str <- readFile "Day14.txt"
  let solvedWalls1 = part1 str
  putStrLn (show $ (n solvedWalls1) - (n $ wallSet str))
  putStrLn "Running part 2..."
  let solvedWalls2 = part2 str
  putStrLn (show $ (n solvedWalls2) - (n  $ wallSet str))

n :: S.Set a -> Int
n = length . S.elems

example = "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9"

parseLine :: [String] -> [String]
parseLine [] = []
parseLine w@(h:t)
 | h == "->" = parseLine t
 | otherwise = h:parseLine t

tuples :: String -> (Int, Int)
tuples x = let (f,s) = split x in (read f, read s)

split :: String -> (String, String)
split str = go str ""
  where
    go (',':xs) res = (res, xs)
    go (a:xs) res = go xs (res ++ [a])

-- list of (Int, Ints) ---- interpolate --->   (Int,Int), depending on (x,y) axis that is changing
interpolated :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
interpolated (m1, n1) (m2, n2) =
  [(a, b) | a <- range m1 m2, b <- range n1 n2 ]

range :: Int -> Int -> [Int]
range m1 m2 = [ a | a <- [min m1 m2 .. max m1 m2]  ]

data CState = CState { walls :: S.Set (Int, Int), pos :: (Int, Int), yMax :: Int }

ws :: String -> [[(Int, Int)]]
ws str = fmap (fmap tuples) $ parseLine . words <$> lines str

wallSet :: String -> S.Set (Int, Int)
wallSet str  = S.fromList $ concatMap (\line -> concatMap (uncurry interpolated) $ zip line (tail line)) (ws str)

findyMax str = S.findMax  $ S.map snd (wallSet str)

part1 :: String -> S.Set (Int, Int)
part1 str = walls . snd $ runState (iterateUntil ((> yMax) . snd) step) (CState { walls=wallSet str, pos=(500,0), yMax=yMax + 2})
   where
     yMax = findyMax str

part2 :: String -> S.Set (Int, Int)
part2 str = walls $ until (S.member (500,0) . walls) (execState step) (CState { walls=wallSet str, pos=(500,0), yMax=findyMax str + 2})

step :: State CState (Int, Int)
step = do
  s@CState {..} <- get
  let (m, n) = pos
  let [d, dl, dr] = [ (m, n+1), (m-1, n+1), (m+1,n+1) ]
  if | validMove d s -> save walls d -- down
     | validMove dl s -> save walls dl -- down left
     | validMove dr s-> save walls dr --down right
     | otherwise -> save (S.insert pos walls) (500,0) -- sand becomes a wall, restart
  where
    save walls p  = p <$ modify (\x ->x{walls=walls, pos=p})

validMove :: (Int, Int) -> CState ->  Bool
validMove p@(x,y) s = S.notMember p (walls s) && y < (yMax s)

propParseLine :: Bool
propParseLine = parseLine (words ("1,2 -> 2,3"))  == ["1,2", "2,3"]

propWallSet :: Bool
propWallSet = (498,4)  `S.member` (wallSet example) && (502,9)  `S.member` (wallSet example)
