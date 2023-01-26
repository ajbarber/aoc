{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
import Prelude
import Control.Arrow ((<<<))
import Control.Lens.At
import Control.Lens.Each
import Control.Lens.Getter
import Control.Lens.Setter
import Control.Lens.TH
import Control.Monad (forM_)
import Control.Monad.State.Strict
import Control.Monad.State.Class
import Control.Monad
import Data.Function
import Data.List.Extra (splitOn)
import Data.Maybe (fromMaybe)
import Data.IntMap qualified as M
import Data.List (uncons)
import Data.Traversable (for, traverse)
import Data.Foldable (foldl', foldMap', for_, traverse_)
import Debug.Trace ( traceShow, trace )

data Monkey = Monkey { _monkeyId :: Int,
                       _startingItems :: [Int],
                       _operation :: Int -> Int,
                       _divisibleBy :: Int,
                       _trueThrowTo :: Int,
                       _falseThrowTo :: Int }
makeLenses ''Monkey

data MonkeyState = MonkeyState { _monkeyMap :: M.IntMap Monkey,
                                 _counts :: M.IntMap Int }
makeLenses ''MonkeyState

main :: IO ()
main = do
  monkeys <- map parse . splitBlocks <$> readFile "day11.txt"
  putStrLn $ show monkeys
  let n = length monkeys
  let monkeyMap = M.fromList $ zip [0..] monkeys
  let intMap = M.fromList $ zip [0..n] (replicate n 0)
  putStrLn . show $ (execState (replicateM_ 20 doRoundL) (MonkeyState monkeyMap intMap))

instance Show Monkey where
  show (Monkey id si op db tt ft) = "id:" ++ show id ++ "|" ++
                                    "Starting items" ++ show si ++ "|" ++
                                    "Div by" ++ show db ++ "|" ++
                                    "True throw to monkey" ++ show tt ++ "|" ++
                                    "False throw to monkey" ++ show ft

instance Show MonkeyState where
  show (MonkeyState mm cts) = show mm <> show cts

doRoundL :: MonadState MonkeyState m => m ()
doRoundL = do
  keys <- uses monkeyMap M.keys
  forM_ keys (\k -> inspectL k)

-- using lenses allows us to write the inspect function almost
-- completely imperatively
inspectL :: M.Key -> MonadState MonkeyState m => m ()
inspectL k = do
  m@Monkey {..} <- uses monkeyMap (M.! k) -- uses is similar to gets in mtl
  counts . at k . each += length _startingItems
  -- {..} exposes all the fields of the record in this scope. Only _startingItems is used
  for_ (_startingItems) (\i -> do
    let worryLevel = worry m i
    -- drop first held item
    monkeyMap . at k . each . startingItems %= maybe [] snd . uncons
    -- pass first held item off
    if worryLevel `mod` _divisibleBy == 0
    then monkeyMap . at _trueThrowTo . each . startingItems %= (++ [worryLevel])
    else monkeyMap . at _falseThrowTo . each . startingItems %= (++ [worryLevel]))

worry :: Monkey -> Int -> Int
worry Monkey {..} i = _operation i `div` 3

splitBlocks :: String -> [String]
splitBlocks = splitOn "\n\n"

test = S.singleton

parse :: String -> Monkey
parse block = foldr parseLn (Monkey 0 [] id 0 0 0) (lines block)
  where
    parseLn :: String -> Monkey -> Monkey
    parseLn line b = case (words line) of
      ["Monkey",id] ->  b{_monkeyId = read (take 1 id)}
      ("Starting":_:arr) -> b{_startingItems = map (read . filter (/= ','))  arr}
      ["Operation:", "new", "=", "old", s, "old"] -> b{_operation = \old -> (parseSign s) old old}
      ["Operation:", "new", "=", "old", s, d] -> b{_operation = \old -> (parseSign s) old (read d)}
      ["Test:", "divisible", "by", n] -> b{_divisibleBy = read n}
      ["If", "true:", "throw", "to", "monkey", i] -> b{_trueThrowTo = read i}
      ["If", "false:", "throw", "to", "monkey", i] -> b{_falseThrowTo = read i}
      _ -> b
    parseSign = \x -> case x of
      "+" -> (+)
      "*" -> (*)
