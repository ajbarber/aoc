{-# LANGUAGE BlockArguments #-}
module Day4 where
import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad
import Data.Maybe
import Data.List

type Grid = M.Map (Int, Int) Char

main :: IO ()
main = do
  str <- readFile "Day4.txt"
  let st = execState (parse str) M.empty
  let part1 = allMatches st
  print part1

parse ::  String -> State (M.Map (Int,Int) Char) ()
parse str = do
  forMi_ (lines str) \(i, line) -> do
    forMi_ line \(j, c) -> do
      modify (M.insert (i,j) c)

forMi_ :: (Monad m, Enum a0, Num a0) => [b1] -> ((a0, b1) -> m b) -> m ()
forMi_ xs = forM_ (zip [0..] xs)

diag :: Grid -> (Int, Int) -> (Int -> Int -> Int) -> String
diag g (i, j) rtol = catMaybes $ takeWhile isJust [ M.lookup ((+) i k,rtol j k) g | k <- [0..]]

vert :: Grid -> (Int, Int) -> String
vert g (i,j) = catMaybes $ takeWhile isJust [ M.lookup (i+k,j) g | k <- [0..]]

hori :: Grid -> (Int, Int) -> String
hori g (i,j) = catMaybes $ takeWhile isJust [ M.lookup (i,j+k) g | k <- [0..]]

diags :: Grid -> [String]
diags g = let ((m,n),_) = M.findMax g in
   ([diag g (0,l) (+)   | l <- [0..n] ] <>
    [diag g (k,0) (+)   | k <- [1..m] ] <>
    [diag g (0,n-l) (-) | l <- [0..n] ] <>
    [diag g (k,n) (-)   | k <- [1..m] ])

verts :: Grid -> [String]
verts g = takeWhile (/="") [vert g (0,l) | l <- [0..]]

horis :: Grid -> [String]
horis g = takeWhile (/="") [hori g (k,0) | k <- [0..]]

substrs :: String -> String -> Int
substrs strn@(s:str) substr = let (cmp, rest) = splitAt (length substr) strn in
  if substr /= cmp then substrs str substr
  else 1 + substrs rest substr
substrs [] _ = 0

check :: String -> String -> Int
check subStr fullStr = let
  x = substrs fullStr subStr
  y = substrs (reverse fullStr) subStr
  in x + y

allMatches g = sum $ (check "XMAS") <$> (diags g <> horis g <> verts g)
