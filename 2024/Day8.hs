{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}

module Day8 where
import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad (forM_)
import Debug.Trace (traceShow, trace)
import Data.List (nub)

(&|&) = liftA2 (&&)

main :: IO ()
main = do
  str <- readFile "Day8.txt"
  let st = execState (parse str) M.empty
  let elems = nub $ filter ((/='.') &|& (/='#')) $ fst <$> (M.elems st)
  print $ "elems" <> (show elems)
  let graph = foldr (\a m -> M.foldrWithKey (const . step a) m m) st elems
  let part1 = sum $ snd <$> (M.elems graph)
  print ("Part 1:" <> show part1)

type Graph = M.Map (Int, Int) (Char, Int)

parse ::  String -> State (M.Map (Int,Int) (Char,Int)) ()
parse str = do
  forMi_ (lines str) \(i, line) -> do
    forMi_ line \(j, c) -> do
      modify (M.insert (i,j) (c,0))

forMi_ :: (Monad m, Enum a0, Num a0) => [b1] -> ((a0, b1) -> m b) -> m ()
forMi_ xs = forM_ (zip [0..] xs)

--- if antenna then search others on the graph,
--- reflect their distance and plonk a # down at
--- the reflected point
step :: Char -> (Int, Int) -> Graph -> Graph
step elem (i, j) g = let v = fst <$> M.lookup (i, j) g in
  if | Just elem /= v  ->
       let dists = map (\(m,n) -> ((m-i), (n-j))) (findOthers elem g)
           anyTwoFactor = length $ filter pairFactorTwo (combinations 2 dists) in
       if anyTwoFactor > 0 then M.adjust (\(x,ct) -> (if x == '.' then '#' else x, 1)) (i,j) g else g
     | otherwise -> g

pairFactorTwo :: [(Int,Int)] -> Bool
pairFactorTwo pair = let (dm, dn) = pair !! 0
                         (dp, dq) = pair !! 1 in ((dm `divMod2Safe` dp) && (dn `divMod2Safe` dq)) ||
                                                 ((dp `divMod2Safe` dm) && (dq `divMod2Safe` dn))

findOthers :: Char -> Graph -> [(Int, Int)]
findOthers x g = M.keys $ M.filter ((==x) . (fst)) g

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ (combinations n xs)

divMod2Safe :: Int -> Int -> Bool
divMod2Safe x y  =
  if y ==0 && x==0 then True
  else if y==0 && x /= 0 then False
  else x `divMod` y == (2,0)
