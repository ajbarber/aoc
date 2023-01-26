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
  putStrLn "Running..."
  str <- readFile "Day14.txt"
  let solvedWalls = solve str
  putStrLn (show $ (length $ S.elems solvedWalls) - (length $ S.elems (wallSet str)))

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

data CState = CState { walls :: S.Set (Int, Int), pos :: (Int, Int) }

ws :: String -> [[(Int, Int)]]
ws str = fmap (fmap tuples) $ parseLine . words <$> lines str

wallSet :: String -> S.Set (Int, Int)
wallSet str  = S.fromList $ concatMap (\line -> concatMap (uncurry interpolated) $ zip line (tail line)) (ws str)

solve :: String -> S.Set (Int, Int)
solve str = walls . snd $ runState (iterateUntil ((> mMax) . snd) step) (CState { walls=wallSet str, pos=(500,0)})
   where
      mMax = S.findMax  $ S.map snd (wallSet str)

step :: State CState (Int, Int)
step = do
  CState {..} <- get
  let (m, n) = pos
  let [d, dl, dr] = [ (m, n+1), (m-1, n+1), (m+1,n+1) ]
  if | S.notMember d walls -> save walls d -- down
     | S.notMember dl walls -> save walls dl -- down left
     | S.notMember dr walls -> save walls dr --down right
     | otherwise -> trace "saving wall" save (S.insert pos walls) (500,0) -- sand becomes a wall, restart
  where
    save walls p = trace ("saving" <> show p) p <$ (put $ CState walls p)

propParseLine :: Bool
propParseLine = parseLine (words ("1,2 -> 2,3"))  == ["1,2", "2,3"]

propWallSet :: Bool
propWallSet = (498,4)  `S.member` (wallSet example) && (502,9)  `S.member` (wallSet example)
