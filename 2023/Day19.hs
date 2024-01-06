module Day19 where

import Prelude
import Data.List.Extra
import qualified Data.Map.Strict as M
import Data.Maybe
import Control.Monad
import Debug.Trace

type Instructions = M.Map String [(Maybe Char, Maybe Int -> Bool, String)]
type Values = M.Map Char Int

(|||) = liftM2 (||)

main :: IO ()
main = do
  str <- readFile "Day19.txt"
  let (instructions, values) = parse (lines str)
  let part1 = sum $ snd <$> filter ((=="A") . fst) (workflows values "in" instructions)
  print ("Part 1 " <> show part1)

parse :: [String] -> (Instructions, [M.Map Char Int])
parse lines = let (firstBit, secondBit) = trace (show (head lines)) span (/="") lines in
  (trace (show firstBit) foldl' parseLine M.empty firstBit, toMap M.empty <$> tail secondBit)

toMap :: M.Map Char Int -> String -> M.Map Char Int
toMap m str = foldr addMap m (splitOn "," (withoutBraces str))
   where
     addMap a = M.insert (head a) (read $ drop 2 a)

withoutBraces :: String -> String
withoutBraces str = drop 1 $ take (length str - 1) str

-- a<2006:qkq
parseInstruction :: String -> (Maybe Char, Maybe Int -> Bool, String)
parseInstruction str = go (splitOn ":" str)
  where
    go [s] = (Nothing , const True, s)
    go (f:s:_) = (Just (head f), parseFn (tail f), s)

parseFn :: String -> (Maybe Int -> Bool)
parseFn ('>':operand) = isNothing ||| (\(Just x) -> x > read operand)
parseFn ('<':operand) = isNothing ||| (\(Just x) -> x < read operand)

-- px{a<2006:qkq,m>2090:A,rfg}
parseLine :: Instructions -> String -> Instructions
parseLine m str = let key = takeWhile (/='{') str
                      instrs = parseInstruction <$> wordsBy (==',') (withoutBraces $ dropWhile (/='{') str)
                      in M.insert key instrs m

-- Lookup e.g qqz,
-- peek at head rule: (s, fn, qs) ->
--   if result of fn true -> return [.. qqz]
--   else reapply with head rule popped off
--
-- px{a<2006:qkq,m>2090:A,rfg}
-- pv{a>1716:R,A}

workflows :: [Values] -> String  -> Instructions -> [(String, Int)]
workflows values cur instr = map (\a -> (workflow a cur instr [], sum (M.elems a))) values

workflow :: Values -> String -> Instructions -> [String] -> String
workflow values cur instr acc = case M.lookup cur instr of
  Just ((key, fn, goto):instrs) -> if fn ((values M.!) <$> key)
                                          then workflow values goto instr (acc <> [cur])
                                          else workflow values cur (pop cur instr) acc
  Nothing -> cur

pop :: String -> Instructions -> Instructions
pop k' = M.mapWithKey (\k a -> if k == k' then tail a else a)
