module Day12 where
import Data.List.Extra
import Data.Maybe
import qualified Data.Map as M
import Control.Monad.State.Strict
import Control.Applicative ((<|>))

main :: IO ()
main = do
  str <- readFile "Day12.txt"
  let parsed = parse <$> lines str
  --let part1 =  take 50 $  map (snd .  process' M.empty "") parsed
  let part1 = sum $ map (uncurry (flip waysAns)) parsed
  print part1
  let part2 = sum $ map (uncurry (flip waysAns) . uncurry (unfold 5)) parsed
  print part2

type Rules = [Int]
type Accum = String

parse :: String -> (Accum, Rules)
parse line = let (a:r:_) = words line in
  (a, read <$> wordsBy (==',') r)

unfold :: Int -> Accum -> Rules -> (Accum ,Rules)
unfold n a r = (intercalate "?" (replicate n a), concat $ replicate n r)

--"??????#?#?#?..???#?.",[6,2]
-- Initial brute force tree based approach - fails to generalise to Part 2
-- process' :: String -> (String, Rules) -> MonadMemo _ Int Int
-- process' accum ([],rules) = pure $ length [accum  | compliant accum rules]
-- process' accum (current@('?':cs), rules) = do
--   child1 <- process' (accum++".") (cs, rules)
--   child2 <- process' (accum++"#") (cs, rules)
--   return child1 + child2
-- process' accum (current@(c:cs), rules) = process' (accum<>[c]) (cs, rules)

compliant :: Accum -> Rules -> Bool
compliant accum rules = let
  parsed = filter (/="") $ splitOn "." accum
  ours = map length parsed
  in (ours == rules)

(|||) = liftA2 (||)

{-
-| The 2d map represents a sort of chain starting at (0,0), the first character of the string
   is tested against the first rule (an element of the array of min lengths).
-  Each 2d index into the map represents an index into a string and the rules, and all the permutations possible
-  from that point forward.
-
-   Pseudo code:
              cases:
                 |1. current rule r if current char is = . then M[r,c] <- M[r,c+1]
                 |2. current rule r if current char is # and there is a run (e.g "###.")  M[r,c] = M[r+1, c+r+1], o.w 0
                 |3. current rule r if current char is ? add both operations on M in 1. *and* 2. above

-  Final result is M[0,0]

--------------------------
Example [3,2]

  ?###????..
3 2210000000
2 0000021000

Ans = [0,0] = 2
--------------------------
-}
ways :: Rules -> String -> M.Map (Int, Int) Int
ways rules line = combinations
   where combinations = M.fromList [ ((i,j), combination i j) |
                                               i <- [0..iMax],
                                               j <- [0..jMax] ]
         (iMax,jMax) = (length rules -1, length line -1)
         combination ri ci = let
              r = rules !? ri
              l = drop ci line
              process (Just p) q@('#':_) = takeChars p q
              process (Just p) q@('?':_) = takeChars p q + skip
              process (Just _) q@('.':_) = skip
              skip = fromMaybe 0 $ combinations M.!? (ri, ci+1)
              noMoreHashes u =  notElem '#' $ drop u l
              defaultVal (v, u) = if v > iMax && noMoreHashes u then 1 else 0 -- consumed all rules -> 1
              takeChars r l = if canTake r l then fromMaybe (defaultVal (ri+1,r)) (combinations M.!? (ri + 1, ci + r+1)) else 0
              in process r l

waysAns :: Rules -> String -> Int
waysAns r s =  ways r s M.! (0,0)

canTake :: Int -> String -> Bool
canTake r x = let z = all ((=='#') ||| (=='?')) $ take r x
                  end = safeHead z' == Just '.' || safeHead z' == Just '?' || isNothing (safeHead z')
                  z' = drop r x in z && length x >= r && end

safeHead :: [a] -> Maybe a
safeHead xs = if null xs then Nothing else Just (head xs)
