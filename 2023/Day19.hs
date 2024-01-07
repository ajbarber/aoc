{-# LANGUAGE LambdaCase #-}
module Day19 where

import Prelude
import Data.List.Extra
import qualified Data.Map.Strict as M
import Data.Maybe
import Control.Monad
import Debug.Trace

type Instructions = M.Map String [(Maybe Char, Maybe Int -> Bool, String)]
type InstructionsSimple = M.Map String [(Maybe Char, (Integer, Integer), String)]
type Values = M.Map Char Int

(|||) = liftM2 (||)

main :: IO ()
main = do
  str <- readFile "Day19.txt"
  let (instructions, values) = parse (lines str)
  let part1 = sum $ snd <$> filter ((=="A") . fst) (workflows values "in" instructions)
  print ("Part 1 " <> show part1)
  let instructions' = foldl' parseLine2 M.empty (takeWhile (/="") $ lines str)
  let part2worked = workflow2 "in" instructions' (M.fromList [('x',(1,4000)),('m',(1,4000)),('a',(1,4000)),('s',(1,4000))])
  let part2 = product . M.elems . M.map (\(r1,r2) -> if r2 >= r1 then r2-r1+1 else 0) <$> part2worked
  print (sum part2)

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
parseFn ('>':operand) = isNothing ||| ((> read operand) . fromJust)
parseFn ('<':operand) = isNothing ||| ((< read operand) . fromJust)

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

pop :: Eq k => k -> M.Map k [a] -> M.Map k [a]
pop k' = M.mapWithKey (\k a -> if k == k' then tail a else a)

-- part 2

-- px{a<2006:qkq,m>2090:A,rfg}
parseLine2 :: InstructionsSimple -> String -> InstructionsSimple
parseLine2 m str = let key = takeWhile (/='{') str
                       instrs = parseInstr2 <$> wordsBy (==',') (withoutBraces $ dropWhile (/='{') str)
                       in M.insert key instrs m

parseInstr2 :: String -> (Maybe Char, (Integer, Integer), String)
parseInstr2 str = go (splitOn ":" str)
  where
    go [s] = (Nothing, (1, 4000) , s)
    go (f:s:_) = (Just (head f), parseBoundary (tail f), s)

parseBoundary :: String -> (Integer,Integer)
parseBoundary ('>':operand) = (read operand+1, 4000)
parseBoundary ('<':operand) = (1, read operand-1)
parseBoundary rest = error rest

workflow2 :: String -> InstructionsSimple -> M.Map Char (Integer,Integer) -> [M.Map Char (Integer,Integer)]
workflow2 cur instr acc =  case M.lookup cur instr of
  Just [] -> []
  Just ((Nothing, _, "A"):_) -> [acc]
  Just ((Nothing, _, "R"):_) -> []
  Just ((Nothing, (n1, n2), goto):instrs) ->  workflow2 goto instr acc
  Just ((Just var, (n1, n2), goto):instrs) -> workflow2 goto instr (insert acc goto var (n1, n2)) <>       --branch
                                              (if n1 >= 1 then workflow2 cur (pop cur instr) (insert acc goto var (1, n1-1)) else []) <>
                                              (if n2 <= 4000 then workflow2 cur (pop cur instr) (insert acc goto var (n2+1, 4000)) else [])--pop
  Nothing -> ([acc | cur == "A"])
  where
    insert acc goto 'A' _ = acc
    insert acc goto 'R' _ = acc
    insert acc goto var (n1, n2) = M.alter (\case Just (a1, a2) -> Just (max a1 n1, min a2 n2)
                                                  Nothing -> Just (n1, n2)) var acc
