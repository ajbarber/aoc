module Day3 where

import Prelude
import Control.Monad
import Control.Monad.State
import Data.Traversable
import Control.Monad.Trans.Writer.CPS
import Data.Char

type CharGrid = [((Int, Int), Char)]

data ParseState = ParseState { buf :: String, adjacent :: Bool, ws :: [String] } deriving Show

main :: IO ()
main = do
  str <- readFile "Day3.txt"
  let grid = execWriter $ toAssoc (lines str)
  print grid
  let numbers = execState (collectNumbers grid) $ ParseState { buf="", adjacent=False, ws=[] }
  print $ sum (read <$> ws numbers)

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

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (i,j) = [(i-1,j), (i+1,j), (i,j-1), (i,j+1), (i-1, j-1), (i+1,j+1), (i-1, j+1), (i+1, j-1)]

isAdjacent :: CharGrid -> ((Int, Int), Char) -> Bool
isAdjacent g ((i,j),c) = any (\x -> fmap (not . isDigit) x == Just True && x /= Just '.') $ (`lookup` g) <$> neighbours (i,j)

forMWIndex_ :: (Monad m, Num i, Enum i) => [a] -> ((i,a) -> m b) -> m ()
forMWIndex_ = forM_ . zip [0..]
