{-# LANGUAGE ScopedTypeVariables #-}
module Day23 where

import Prelude

import Control.Arrow
import Control.Applicative
import Control.Monad (replicateM_)
import Control.Monad.State
import Control.Monad.Fail
import Data.Function
import Data.List
import qualified Data.Set as S
import Data.Maybe
import Data.Proxy
import Data.Traversable
import Debug.Trace
import Test.QuickCheck

type Grid = [Row]
type Row = [(Char, (Int,Int))]

data Direction = N | S | E | W | All deriving Show
type Orientations = [ Direction ]
type Elves = S.Set (Int, Int)
type Round = (Elves, Orientations)

main :: IO ()
main = do
  let matrix = lines example
  let grid = gridToSet (parsed matrix) S.empty
  putStrLn $ show grid
  putStrLn $ show matrix
  let finalSt = execState (replicateM_ 10 runRound) (grid, [N, S, W, E])
  putStrLn . show $ finalSt
  putStrLn . show $ countEmpties finalSt
  pure ()

mapWithIndex :: (Num b1, Enum b1) => (a -> b1 -> b2) -> [a] -> [b2]
mapWithIndex fn xs = uncurry fn <$> zip xs [0..]

-- add the column number to the RHS tuple in each row
cols :: [a] -> [(a, Int -> (Int, Int))]
cols = let ft = flip (,) in mapWithIndex (flip (ft . ft))

-- | as we iterate each row add the row number by applying with @i@
--   to the output of cols
parsed :: [[Char]] -> Grid
parsed = mapWithIndex (\row i -> fmap ($ i) <$> cols row)

parse :: String -> Grid
parse = parsed . lines

-- | Set with coords of all elves
step :: S.Set (Int, Int) -> Row -> S.Set (Int, Int)
step b xys = foldr S.insert b $ snd <$> filter ((=='#') . fst)  xys

gridToSet :: Grid -> S.Set (Int, Int) -> S.Set (Int, Int)
gridToSet gr s = foldl' step s gr

neighbours :: (Int, Int) -> Direction -> [(Int,Int)]
neighbours (x,y) dir = [adjacent | dx <- xDirs, dy <- yDirs,
                        let adjacent = (x + dx, y + dy), not (dx == dy && dx == 0)]
    where
      (xDirs, yDirs) = case dir of
        W -> ([-1,0,1],[-1])
        S -> ([1],[-1,0,1])
        E -> ([-1,0,1],[1])
        N -> ([-1],[-1,0,1])
        All -> ([-1,0,1],[-1,0,1])

move :: (Int, Int) -> Direction -> (Int, Int)
move (m,n) N  = (m-1,n)
move (m,n) S  = (m+1,n)
move (m,n) W  = (m,n-1)
move (m,n) E  = (m,n+1)

proposedMove :: Elves -> [Direction] -> (Int, Int) -> Maybe ((Int, Int), (Int, Int))
proposedMove elves ds e = foldr (<|>) Nothing $ map (\d -> if hasNeighbours elves e d
                                                           then Nothing
                                                           else Just (e, move e d)) ds

elfWillMove :: Elves -> (Int, Int) -> Bool
elfWillMove elves e = hasNeighbours elves e All

finalPropMoves :: (Elves, [Direction]) -> [((Int, Int), (Int, Int))]
finalPropMoves (es,os) = catMaybes $ proposedMove es os <$> S.toList (S.filter (elfWillMove es) es)

uniq :: [((Int,Int), (Int, Int))] -> [((Int,Int), (Int, Int))]
uniq moves = filter (isUniqueMove (snd <$> moves) . snd) moves

runRound :: State Round ()
runRound = do
  st@(elves, os) <- get
  put (foldr (\(old,new)-> S.insert new . S.delete old) elves (uniq $ finalPropMoves st), tail os ++ [head os])

isUniqueMove :: [(Int, Int)] -> (Int, Int) -> Bool
isUniqueMove xs x = length  (filter (==x) xs) == 1

hasNeighbours :: Elves -> (Int, Int) -> Direction -> Bool
hasNeighbours elves e d = foldr ((||) . flip S.member elves) False (neighbours e d)

countEmpties :: Round -> Int
countEmpties (elves,_) =
  let xMin = S.findMin xs
      xMax = S.findMax xs
      yMin = S.findMin ys
      yMax = S.findMax ys
   in (yMax - yMin + 1) * (xMax - xMin + 1) - length (S.elems elves)
   where
     ys = S.map fst elves
     xs = S.map snd elves

-- Tests ---------------------------------------------------------------------------------------------

propNeighbours :: Bool
propNeighbours = neighbours (2,2) N == [(1,1), (1,2), (1,3)] &&
                 neighbours (0,0) E == [(-1,1), (0,1), (1,1)] &&
                 neighbours (0,0) S == [(1,-1), (1,0), (1,1)] &&
                 neighbours (0,0) W == [(-1,-1), (0,-1), (1,-1)]

-- I can't get this to generate arbitrary directions
instance Arbitrary Direction where
  arbitrary = do
    a :: a <- arbitrary
    return a

testDirs = [N, S, E, W]

-- Just case - all neigbours of dest location are not occupied for at least one direction
-- Nothing case - if no move all neigbours are occupied for every direction
propProposedMoveDir :: Elves -> (Int,Int) -> Bool
propProposedMoveDir elves elf = case proposedMove elves testDirs elf of
  Just (f,s) -> foldr (||) False $ (\a -> all (flip S.notMember elves) (neighbours s a)) <$> testDirs
  Nothing -> foldr (&&) True $ (\dir -> any (flip S.member elves) (neighbours elf dir)) <$> testDirs

-- all '#' in input matrix resolve 1:1 with elf locations
propElfLocations :: Bool
propElfLocations = let
  matrix = lines example
  grid = gridToSet (parsed matrix) S.empty in
  all (\(m,n) -> matrix !! m !! n == '#') (S.toList grid) &&
  (not $ any (\(m,n) -> matrix !! m !! n /= '#') (S.toList grid))

-- simple proposed Moves when no neighbours
propProposedMoveSimple :: Bool
propProposedMoveSimple = proposedMove mempty [N] (1,1) == Just ((1,1),(0,1)) &&
                     proposedMove mempty [S] (1,1) == Just ((1,1),(2,1)) &&
                     proposedMove mempty [W] (1,1) == Just ((1,1),(1,0)) &&
                     proposedMove mempty [E] (1,1) == Just ((1,1),(1,2))

-- example given on AoC web page
propFinalMoveNeighbours :: Bool
propFinalMoveNeighbours = (uniq . finalPropMoves) (S.fromList [(1,2),(1,3),(2,2), (4,2),(4,3)], [N, S, W, E]) ==
                          [((1,2),(0,2)),((1,3),(0,3)),((4,3),(3,3))]

-- Start of round 1
--    0
-- ..............
-- ..............
-- .......#...... (0,4)
-- .....###.#.... (1,2), (1,3), (1,4), (1,6)
-- ...#...#.#.... (2,0), (2,4), (2,6),
-- ....#...##.... (3,1) (3,5), (3,6),
-- ...#.###...... (4,0), (4,2), (4,3), (4,4)
-- ...##.#.##.... (5,0), (5,1), (5,3), (5,5), (5,6)
-- ....#..#...... (6,1), (6,4)
-- ..............
-- ..............
-- ..............

-- End of round 1
--    0
-- ..............
-- .......#...... (-1,4)
-- .....#...#.... (0,2), (0,6)
-- ...#..#.#..... (1,0), (1,3), (1,5)
-- .......#..#... (2,4), (2,7)
-- ....#.#.##.... (3,1), (3,3), (3,5), (3,6)
-- ..#..#.#...... (4,-1), (4,2), (4,4)
-- ..#.#.#.##.... (5,-1), (5,1), (5,3), (5,5), (5,6)
-- ..............
-- ....#..#...... (7,1), (7,4)

-- End of round 2
--    0
-- ..............
-- .......#...... (-1,4)
-- ....#.....#... (0,1) , (0,7)
-- ...#..#.#..... (1,0), (1,3), (1,5)
-- .......#...#.. (2,4), (2,8)
-- ...#..#.#..... (3,0), (3,3), (3,5)
-- .#...#.#.#.... (4,-2), (4,2), (4,4), (4,6)
-- ..............
-- ..#.#.#.##.... (6,-1), (6,1), (6,3), (6,5), (6,6)
-- ....#..#...... (7,1), (7,4)
-- ..............
-- ..............

--example :: String
--example = "....#..\n..###.#\n#...#.#\n.#...##\n#.###..\n##.#.##\n.#..#..\n"

example :: String
example = ".#.##.#...###.#.#.#.#.#....##....#...##...##..#.##..#..#.#.#.###..#....\n\
\.....#.#..####.......#.###.....##.#..#.###.#.#..##.#.#.#.#..##.#.######\n\
\#.####.##..####.#####.#.##.#..##.#.#.###.######.#.#.##..#####.#.##..##.\n\
\#..#.####...#..##...#.#####.######.#...#....##..##..#......##.#####..##\n\
\###..#.##..#########...#..####.#.#..##..#.#.#..###..##.###...##.#.#.#..\n\
\..#..##.##.###..###.###...#.##..##.#####..#.###.....##...##......##.#..\n\
\.###.#..#.#..#..#.##..#...##.#...#...##...##.####....#....#####..####.#\n\
\..#####...#..###...#..#.#.#.#..#....#.##.#.#....#.#.#.#.....#.#...###..\n\
\.#########.#...###.######..####.#..##..##.##......#..####....##.#....#.\n\
\###.#.#....#..#.###..#....##...#..#.##.....#####...##.##...###......#..\n\
\.##.##.###.##.###..#..#..##.##.####...#..#.....##..####.##.##...#.##..#\n\
\..#..##.#.#...##.#.####.#.#####.########....##.###..#......##.#.####.##\n\
\..######...#.#.#####.##...####...##...######.####..##.####...#####.##..\n\
\########....##.#.#.#.....#.#.##.#.#...#.#.##..#..#....#...##.##..##..#.\n\
\########..#..#.##.##.######...##.##.#..#.###.#..#.#...###...#.##.#####.\n\
\#.##..#.##.##..###.##..########.#...###.#.#.###...####..#######.###...#\n\
\.###.#.#######.#..#.#.#..#...##..#...##.######..###...#.#.#.#.###.#.###\n\
\..###.#..###...#....#..###...##..###.#.#.#.#..##.....##...####..####..#\n\
\..####..#..#...#####.#.#.......#.#####.###.##.#..#.#.......#.#.#.##.##.\n\
\..##...####.#..###.##..#########.....#....#.#########.#.#...##.##.#....\n\
\###..###.#...#...##.##..###..#####..#...#####.##..#.#.###.#.###..###..#\n\
\...##...#...##..####.##.#.#..##.###.########.#..#..##.###.#.##.#.#....#\n\
\...##.###.######..#.###.####.###..#..#.#....#.##.#.#.##..#..##.#.#.####\n\
\######.####...#.....#.#..#..##.#...##.......##.#...######.......##..###\n\
\.##..#...##..##.####.###.#.####....####...###.##..###.#.#####.##.......\n\
\.##.#.###...#.###..###....#..###...##.#.###.#..###...##.....#..##.###..\n\
\.##.#......#.#....#......#.##....###........##..####....#.#..#...#.....\n\
\##..#..##..#...#.####..#.##..#####.#.##....##.#####.....###.#...#.###..\n\
\#...#..#.#..####.#.#..##.###...##..##...#......#.......##....#..##..#.#\n\
\.#..###..##.###......##..####..#.####.#........##.##.#..##.##..#.##..#.\n\
\#######.#.#.###.##..##..##.####......#....#.#.#.##.#.#.##.#.###.##.###.\n\
\#....#...#.#....###....#...#.#.....#...#.#####.#.......###.#.#..##..##.\n\
\.#.#####..#....####.####..#..#....#....#####.##...#.#..##.#..#.#..#.#.#\n\
\..##.#.###..##.##....##..####.#.#.#....#####.###..#####.#.#...#..##...#\n\
\...#.##.#.#...#.#...#...##.##..#..###....#.#.#.##...#..####...#..##.#.#\n\
\..##.#...#...####.#####..#.##.#.#.#..##.....#.##..##....#.....###..####\n\
\#.######.###....######.##..#.#.###.##.###..#..####.#...#.#...#.#.##...#\n\
\..#.##.#.#####.#....#.#.#..#.####.#..##.#..####.....#..#...##...####.#.\n\
\.#..#.....####.#####.#.#.#.....##.##..##.#.#.#..#.....#....#...#.#..#..\n\
\....##.##.#..##.##.###.#.......####.#.###..#..##..###.##....#.#.###....\n\
\##.#.#.##.##..###..###.#.##.#..#....#.###..#####..##...###..#.##...##..\n\
\##..##.##..##..#.##.##...#.###.#.##########...##.##########...###....#.\n\
\.####.#.###.##.#####...###.#..##.####...##.##.#.###...##.######.#.#.###\n\
\#..##.#..##........#..#.#######..........###..#..###.###.#.#.###.#..##.\n\
\.#..###.#...###.####..#..#..#.#.###.##.#..##...#####.....###.......#..#\n\
\#.##.####..###..#......#.##...#..#..#..#.....#.#....##.####..#.###.##.#\n\
\.#.###..###.##.###.#.#.##.####.###..#..#..######.###.#...##..#.##..#..#\n\
\#.#..#.........#.####..#....#.#....#.#####..##..#..#..#.....###.#..####\n\
\.#.#....####.##.##.#...#......#.#.#.....#....#....#.##..#..##.#..##.###\n\
\##...#..######........##.#...#####..#.#.#.####..#####.#.#...#.##..###..\n\
\.##.####.#.##.##....#.#.#...#.#####..##......#.#.##.#.#.###.#......#..#\n\
\############...##.##.#..#.#...#######.#....###....####..#.#######.#...#\n\
\#.#.#..####..#.#.#..##......###.###...##.#..##..########.##.###..#..##.\n\
\#.#.##.#.####..#.###.###.#.#.##.#....#######..###.#..##.#...#.###.###.#\n\
\...#.#.....####....#...#.#.#....#..###..#.#.#.###..##.#..#...#..##..#.#\n\
\.##.##...#...####....#....#.#.###..##.##..####.#..#.###.#####..##...#..\n\
\...#..#..###..#...#######.#.###.##...#....####.#.#..##.##...#...#...###\n\
\....#..#.#.#.##..#.##.#.###...###.....#.#..##.#.##..#.##.###..#.#.###.#\n\
\####.#######.#.#.###...##.#..##..#.#...###..#...#.###.###....#...#...##\n\
\.###.#.#.#...#....##.####.#....#.....#.#..##.##.##.#.##.#..#.#.......##\n\
\#..###.##..###.##...###..#..##.#.#...##..#.#........#.###..######...#.#\n\
\..######.###..###.#......#.#.###.####.#...#.#.##.#.......##..#.##....#.\n\
\#.###.#.#.###.#.#.##.........##...#....#.###..#...#.#..#.##....#..##..#\n\
\.##.#..#.#....#...#..###........#..#..##..##.#...#.###...########.##.##\n\
\.##...#.....#.###.##....#.#..#..####.###...##...#..###...##..##.#####..\n\
\#######..#.#.#.####.##..#######.#..##.#...###.....###.#..####.#.###.#.#\n\
\.##..#...##.....##..###.##..##.###########.####..##.......#...#.###.#..\n\
\....##....##.###.####...#..#....##.#.#.#...####.##..#...#.......##..#.#\n\
\##..##.##.#..##.#...##.....#.##..#.#.##.##.#...#...#.##..#.#.#.#.#.##..\n\
\##..##.##...##..##.###..##.##..########..##......#.#..#..###..#####..#.\n\
\.#.##..#..##.######.#.#....##.###..#..#.#...#.#....####.#####.#.#..###."
