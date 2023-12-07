module Day7 where

import Prelude
import Data.Foldable
import Data.Maybe
import Data.List

main :: IO ()
main = do
  str <- readFile "Day7.txt"
  let games = parse str
  let played = zip games (play . fst <$> games)
  let part1 = sum $ zipWith (*) [1..] (snd . fst <$> sortOn (\(gd, hand) -> (hand, fst gd)) played )
  mapM_ putStrLn (take 100 $ drop 400 $ show <$> played)
  let played2 = zip games (play . bestSubstitution .fst <$> games)
  let part2 = sum $ zipWith (*) [1..] (snd . fst <$> sortOn (\(gd, hand) -> (hand, fst gd)) played2 )
  print part1
  print part2

-- default Ord instance of an ADT is the order listed below
-- default Ord on Tuple and Lists is lexographic, so we get sorting for free with `sortOn`
data Card = CardJ | Card2 | Card3 | Card4 | Card5 | Card6 | Card7 | Card8 | Card9 | CardT | CardQ | CardK | CardA deriving (Eq, Ord, Show)

toCard :: Char -> Card
toCard '2' = Card2
toCard '3' = Card3
toCard '4' = Card4
toCard '5' = Card5
toCard '6' = Card6
toCard '7' = Card7
toCard '8' = Card8
toCard '9' = Card9
toCard 'T' = CardT
toCard 'J' = CardJ
toCard 'Q' = CardQ
toCard 'K' = CardK
toCard 'A' = CardA

parse :: String -> [([Card], Int)]
parse str = parseLine <$> lines str

parseLine :: String -> ([Card], Int)
parseLine str = let (f:s:_) = words str in (toCard <$> f,read s)

substrings :: Int -> [a] -> [[a]]
substrings i l@(x:xs)
     | length l <= i = [l]
     | i == 1 = map (: []) l
     | otherwise = map (x:) (substrings (i-1) xs) <> substrings i xs
substrings i [] = replicate i []

fiveOfaKind :: [Card] -> Maybe Int
fiveOfaKind s@(x:xs) = if all (==x) s then Just 7 else Nothing

same :: [Card] -> Maybe Card
same l@(x:xs) = if all (==x) l then Just x else Nothing

fourOfaKind :: [Card] -> Maybe Int
fourOfaKind str = 6 <$ msum (map same (substrings 4 str))

fullHouse :: [Card] -> Maybe Int
fullHouse str = do
  x <- msum $ map same (substrings 3 str)
  case filter (/=x) str of
    (a:b:_) -> if a==b then Just 5 else Nothing
    _ -> Nothing

threeOfaKind :: [Card] -> Maybe Int
threeOfaKind str = 4 <$ msum (map same (substrings 3 str))

twoPair :: [Card] -> Maybe Int
twoPair str = do
  x <- msum $ map same (substrings 2 str)
  case filter (/=x) str of
    (a:b:c:_) -> if oneDiffering a b c then Just 3 else Nothing
    _ -> Nothing
  where
    oneDiffering a b c = a==b && b /=c || b == c && a /= c || a==c && a/=b

onePair :: [Card] -> Maybe Int
onePair str = do
  x <- msum $ map same (substrings 2 str)
  case filter (/=x) str of
    (a:b:c:_) -> if a/=b && b /=c then Just 2 else Nothing
    _ -> Nothing

highCard :: [Card] -> Maybe Int
highCard str = 1 <$ Just (length (nub str) == length str)

play :: [Card] -> Maybe Int
play row =  msum $ ($ row) <$> [fiveOfaKind, fourOfaKind, fullHouse, threeOfaKind, twoPair, onePair, highCard]

cards = [ Card2, Card3, Card4, Card5, Card6, Card7, Card8, Card9, CardT, CardQ, CardK, CardA ]

substitutions :: [Card] -> [[Card]]
substitutions (CardJ:xs) = concatMap (\c -> (c:) <$> substitutions xs) cards
substitutions (x:xs) = map (x:) (substitutions xs)
substitutions [] = [[]]

bestSubstitution :: [Card] -> [Card]
bestSubstitution card = last $ sortOn (\x -> (play x, x)) (substitutions card)
