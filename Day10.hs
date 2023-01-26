import Prelude
import Control.Monad.State
    ( forM_, modify, execState, MonadState(get), State )
import Data.Char ()
import Data.List ( find )
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe ( isJust )
import Debug.Trace ()
data Instr = Add Int | Noop deriving Show

main :: IO ()
main = do
  str <- readFile "Day10.txt"
  putStrLn "Waiting for puzzle input:"
  putStrLn (show . solve . parse $ str)
  putStrLn "Finished"

instr :: [String] -> Instr
instr ("addx" : val : _) = Add v
   where
      v = read val
instr ("noop" : _) = Noop
instr _  = error "bad instruction"

parse :: String -> [Instr]
parse = map (instr . words) . lines

-- | State will be a tuple of (cycle, register value)
type ProgState = NonEmpty (Int, Int)

step :: Instr -> State ProgState ()
step instr = do
  (c,x) :| _ <- get
  case instr of
    Noop -> modify ((c + 1, x) NE.<|)
    Add y -> modify ((c + 2, x + y) NE.<|)
  pure ()

mark :: Int -> Int -> Bool
mark old new = isJust $ find (\x -> old < x && new >=x) cycles

cycles :: [Int]
cycles = [ 20, 60,  100, 140, 180, 220 ]

solve :: [Instr] -> Int
solve instrs = sum $ cyclePoints (NE.toList (flip execState (NE.singleton (1,1)) $ forM_ instrs step)) (reverse cycles)

-- our register values are in reverse order
-- go backwards and find the cycle start value. first value less than or equal
-- to the cycle point, e.g 219 for the 220 cycle.
cyclePoints :: (Num a, Ord a) => [(a, a)] -> [a] -> [a]
cyclePoints xs [] = []
cyclePoints xs (c:cs) =  c * snd (head xs') : cyclePoints xs' cs
   where
     xs' = dropWhile ((>c) . fst) xs
