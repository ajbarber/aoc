{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Day11 where

import Prelude
import Control.Arrow ((<<<))
import Control.Lens.At
import Control.Lens.Each
import Control.Lens.Getter
import Control.Lens.Setter
import Control.Lens.TH
import Control.Monad.State.Strict
import Control.Monad.State.Class
import Control.Monad.Trans.Writer.Strict
import Control.Monad
import Data.Function
import Data.List.Extra (splitOn)
import Data.Maybe (fromMaybe)
import qualified Data.IntMap as M
import Data.List (uncons, sort)
import Data.Traversable (for, traverse)
import Data.Foldable (foldl', foldMap', for_, traverse_)
import Debug.Trace ( traceShow, trace, traceM )

data Monkey = Monkey { _monkeyId :: Int,
                       _startingItems :: [Integer],
                       _operation :: Integer -> Integer,
                       _divisibleBy :: Integer,
                       _trueThrowTo :: Int,
                       _falseThrowTo :: Int }
makeLenses ''Monkey

data MonkeyState = MonkeyState { _monkeyMap :: M.IntMap Monkey,
                                 _counts :: M.IntMap Integer }
makeLenses ''MonkeyState

instance Eq Monkey where
  (==) m1 m2 = _startingItems m1 == _startingItems m2 && _monkeyId m1 == _monkeyId m2

main :: IO ()
main = do
  monkeys <- map parse . splitBlocks <$> readFile "2022/Day11.txt"
  let n = length monkeys
  print n
  let monkeyMap = M.fromList $ zip [0..] monkeys
  let intMap = M.fromList $ zip [0..n] (replicate n 0)
  let initState = MonkeyState monkeyMap intMap
  let divisor = product $ map _divisibleBy monkeys
  print $ product . take 2 . reverse . sort . M.elems $ _counts (execState (replicateM_ 20 (doRoundL (worry1 3))) initState)
  print $ product . take 2 . reverse . sort . M.elems $ _counts (execState (replicateM_ 10000 (doRoundL (worry2 divisor))) initState)

instance Show Monkey where
  show (Monkey id si op db tt ft) = "id:" ++ show id ++ "|" ++
                                    "Starting items" ++ show si ++ "|" ++
                                    "Div by" ++ show db ++ "|" ++
                                    "True throw to monkey" ++ show tt ++ "|" ++
                                    "False throw to monkey" ++ show ft

instance Show MonkeyState where
  show (MonkeyState mm cts) = show mm <> show cts

isInitialState :: M.IntMap Integer -> Bool
isInitialState im = all (==0) (M.elems im)

doRoundL :: MonadState MonkeyState m => (Monkey -> Integer -> Integer) -> m ()
doRoundL wfn = do
  keys <- uses monkeyMap M.keys
  forM_ keys (\k -> inspectL wfn k)

-- using lenses allows us to write the inspect function almost
-- completely imperatively
inspectL :: (Monkey -> Integer -> Integer) -> M.Key -> MonadState MonkeyState m => m ()
inspectL worry k = do
  m@Monkey {..} <- uses monkeyMap (M.! k) -- uses is similar to gets in mtl
  counts . at k . each += toInteger (length _startingItems)
  -- {..} exposes all the fields of the record in this scope. Only _startingItems is used
  for_ _startingItems (\i -> do
    let worryLevel = worry m i
--    traceM (show worryLevel)
    -- drop first held item
    monkeyMap . at k . each . startingItems %= maybe [] snd . uncons
    -- pass first held item off
    if worryLevel `mod` _divisibleBy == 0
    then monkeyMap . at _trueThrowTo . each . startingItems %= (++ [worryLevel])
    else monkeyMap . at _falseThrowTo . each . startingItems %= (++ [worryLevel]))

worry1 :: Integer -> Monkey -> Integer -> Integer
worry1 d Monkey {..} i = _operation i `div` d

worry2 :: Integer -> Monkey -> Integer -> Integer
worry2 d Monkey {..} i = _operation i `mod` d

splitBlocks :: String -> [String]
splitBlocks = splitOn "\n\n"

parse :: String -> Monkey
parse block = foldr parseLn (Monkey 0 [] id 0 0 0) (lines block)
  where
    parseLn :: String -> Monkey -> Monkey
    parseLn line b = case words line of
      ["Monkey",id] ->  b{_monkeyId = read (take 1 id)}
      ("Starting":_:arr) -> b{_startingItems = map (read . filter (/= ','))  arr}
      ["Operation:", "new", "=", "old", s, "old"] -> b{_operation = \old -> parseSign s old old}
      ["Operation:", "new", "=", "old", s, d] -> b{_operation = \old -> parseSign s old (read d)}
      ["Test:", "divisible", "by", n] -> b{_divisibleBy = read n}
      ["If", "true:", "throw", "to", "monkey", i] -> b{_trueThrowTo = read i}
      ["If", "false:", "throw", "to", "monkey", i] -> b{_falseThrowTo = read i}
      _ -> b
    parseSign = \x -> case x of
      "+" -> (+)
      "*" -> (*)
