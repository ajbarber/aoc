{-# LANGUAGE TupleSections #-}
module Day3 where

import Prelude
import Control.Monad
import Control.Monad.State.Strict
import Data.Traversable
import Control.Monad.Trans.Writer.CPS
import Data.Char
import Data.Maybe
import Data.Function
import Control.Applicative
import Debug.Trace
import Control.Monad.Loops
import Data.Foldable
import Data.List

type CharGrid = [((Int, Int), Char)]

data ParseState = ParseState { buf :: String, adjacent :: Bool, ws :: [String] } deriving Show

main :: IO ()
main = do
  str <- readFile "Day3.txt"
  let grid = execWriter $ toAssoc (lines str)
  print grid
  let numbers = execState (collectNumbers grid) $ ParseState { buf="", adjacent=False, ws=[] }
  let part1 = sum (read <$> ws numbers)
  print ("Part 1:" <> show part1)
  let symbols = filter ((not . isDigit . snd) &.& (('.' /=) . snd)) grid
  let gears = execWriter (gearRatios grid symbols)
  let part2 = sum $ uncurry (*) <$> gears
  print ("Part 2:" <> show part2)

toAssoc :: [[Char]] -> Writer CharGrid ()
toAssoc lines = forMWIndex_ lines (\(i,line) ->
  forMWIndex_ line (\(j,c) -> tell [((i,j),c)]))

collectNumbers :: CharGrid -> State ParseState ()
collectNumbers g = forM_ g (\((i,j),c) -> do
  s <- get
  let adj = isAdjacent g ((i,j),c)
  if isDigit c then put s { buf = buf s ++ [c], adjacent = adjacent s || adj }
  else if adjacent s then put s{ ws = buf s:ws s, buf = "", adjacent=False} --clear buffer
  else put s{ buf="", adjacent=False})

gearRatios :: CharGrid -> CharGrid -> Writer [(Int,Int)] ()
gearRatios g symbols = forM_ symbols (\((i,j),c) -> do
  let nbrs = neighbours (i,j)
  let expansions = expand g . fst <$> filter (isDigit . snd) (mapMaybe (\i -> (i,) <$> lookup i g) nbrs)
  let nbrElems = nubBy ((==) `on` snd) $ expansions
  when (length (fst <$> nbrElems) == 2) $ do
    tell [(read $ (snd <$> nbrElems) !! 0, read $ (snd <$> nbrElems) !! 1)])

expand :: CharGrid -> (Int, Int) -> ((Int, Int),String)
expand g (i,j) = let s = execState (findStart g (i,j)) j in
  ((i,s), snd <$> takeWhile (isDigit . snd) (filter (\((m,n),_) -> i == m && n >= s) g))

findStart :: CharGrid -> (Int, Int) -> State Int Bool
findStart g (i,j) = iterateWhile id (do
   k <- get
   case lookup (i,k-1) g of
     Just r -> if isDigit r then True <$ put (k-1) else pure False
     _ -> pure False)

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (i,j) = [(i+di, j+dj) | di <- [-1..1], dj<-[-1..1], (di,dj) /= (0,0)]

(&.&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(&.&) = liftM2 (&&)

isAdjacent :: CharGrid -> ((Int, Int), Char) -> Bool
isAdjacent g ((i,j),c) = any ((not . isDigit) &.& ('.' /=)) (mapMaybe (`lookup` g) $ neighbours (i,j))

forMWIndex_ :: (Monad m, Num i, Enum i) => [a] -> ((i,a) -> m b) -> m ()
forMWIndex_ = forM_ . zip [0..]
