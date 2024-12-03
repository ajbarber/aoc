module Day3 where

import Data.List.Extra
import Data.Maybe
import Data.Char

main :: IO ()
main = do
  str <- readFile "Day3.txt"
  print str
  let res = sum $ map ((\(a,b) -> a*b) . fromJust) (filter isJust (valid <$> fst (parse str)))
  print res

parse str = foldl' (\(c:cur,acc) a ->
             if a == 'm' && acc then ("m":cur, True)
             else if a == 'm' then ("m":c:cur, True)
             else if acc && a /= ')' then ((c++[a]):cur, True)
             else if acc && a == ')' then ((c++")"):cur,False)
             else (c:cur, acc)) ([""], False) str

valid :: String -> Maybe (Int, Int)
valid ('m':'u':'l':str) = isIntTuple str
valid _ = Nothing

isIntTuple :: String -> Maybe (Int, Int)
isIntTuple tpl = let x = splitOn "," (reverse $ drop 1 $ reverse (drop 1 tpl)) in
  case x of
    [a, b] -> if isInt a && isInt b then Just (read a, read b) else Nothing
    _ -> Nothing

isInt (x:xs) = isDigit x && isInt xs
isInt [] = True
