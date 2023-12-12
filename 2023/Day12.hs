module Day12 where
import Data.List.Extra
import Debug.Trace
import Data.Maybe

main :: IO ()
main = do
  str <- readFile "Day12.txt"
  let parsed = parse <$> lines str
  let part1 =  sum $ map (length . uncurry process) parsed
  print parsed
  print part1

type Rules = [Int]
type Accum = String

parse :: String -> (Accum, Rules)
parse line = let (a:r:_) = words line in
  (a, read <$> wordsBy (==',') r)

process :: Accum -> Rules -> [[String]]
process accum rules
  | not (anyLeft accum) && compliant accum rules = [[accum]]
  | not (anyLeft accum) && not (compliant accum rules) = []
  | otherwise =
      let lstr = replaceFirst accum "."
          rstr = replaceFirst accum "#"
          in process lstr rules <> process rstr rules

anyLeft :: Accum -> Bool
anyLeft str = '?' `elem` str

compliant :: Accum -> Rules -> Bool
compliant accum rules = let
  parsed = filter (/="") $ splitOn "." accum
  ours = map length parsed
  in (ours == rules)

replaceFirst :: Accum -> String -> Accum
replaceFirst accum s = let (h,t) = fromMaybe (accum,"") (stripInfix "?" accum) in h++s++t
