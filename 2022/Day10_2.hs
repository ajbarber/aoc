module Day10_2 where

import Prelude

import Control.Applicative
import Control.Monad.State
    ( modify,
      evalState,
      execStateT,
      MonadState(get),
      MonadTrans(lift),
      State,
      StateT )
import Control.Monad (void, guard)
import Control.Monad.Writer.Class
import qualified Control.Monad.Trans.Writer.CPS as CPS
import Control.Monad.Writer
    ( WriterT, execWriter, execWriterT, Writer )
import Data.Foldable
import Data.List.Extra hiding (enumerate)

data Instr = Add Int | Noop deriving Show

instr :: [String] -> Instr
instr ("addx" : val : _) = Add v
   where
      v = read val
instr ("noop" : _) = Noop
instr _  = error "bad instruction"

parse :: String -> [Instr]
parse = map (instr . words) . lines

main :: IO ()
main = do
  str <- readFile "2022/Day10.txt"
  putStrLn "Part 1:"
  putStrLn (show . part1 . parse $ str)
  putStrLn "Part 2:"
  putStr (show . part2 . parse $ str)
  putStrLn "Finished"

x = "abc"

--  [  Inputs ]     -->    [ Outputs ]  a list indexed by number of cycles with value of x
--    instructions  -->    [ tell into a MonadWriter [a] the value of x, each cycle]
step :: Instr -> WriterT [Int] (State Int) ()
step instr = case instr of
  Add x -> cycle *> cycle *> modify (+x)
  Noop -> cycle
  where
    cycle = do
      x <- get
      tell [x]

xs :: [Instr] -> [Int]
xs instrs = flip evalState 1 $ execWriterT (forM_ instrs step)

frequencies :: [Int]
frequencies = [20, 60, 100, 140, 180, 220]

enumerate :: (Enum a, Num a) => [b] -> [(a, b)]
enumerate = zip [1..]

part1 :: [Instr] -> Int
part1 instrs = sum . map (uncurry (*)) . filter (flip elem frequencies . fst) $ enumerate $ xs instrs

part2 :: [Instr] -> String
part2 instrs = unlines . chunksOf 40 . map (\(i,x) -> if abs (x - (i - 1)`mod` 40) <= 1 then '#' else '.') . enumerate $ xs instrs

-- playing around with WriterT ------------

--- z :: [Int]
-- z = execWriter $ execStateT step 1

-- step ::  StateT Int (Writer [Int]) ()
-- step = do
--   x <- get
--   tell [2::Int]
--   modify (+1)

z2 = flip execStateT 1 $ execWriterT step2

step2 :: WriterT [Int] (State Int) ()
step2 = do
  x <- get
  tell [2::Int]
  modify (+1)
