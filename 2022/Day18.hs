import Prelude

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Char
import Data.List.Extra
import Data.Maybe
import Data.Set qualified as S
import Data.Sequence qualified as SQ
import Data.Foldable
import Debug.Trace
import Test.QuickCheck

main :: IO ()
main = do
  str <- readFile "Day18.txt"
  --let str = example
  putStrLn $ show $ part1 (parsed str)
  putStrLn "Solving part 2"
  putStrLn $ show $ part2 (parsed str)

example :: String
example = "2,2,2\n1,2,2\n3,2,2\n2,1,2\n2,3,2\n2,2,1\n2,2,3\n2,2,4\n2,2,6\n1,2,5\n3,2,5\n2,1,5\n2,3,5"

type Point = (Int, Int, Int)

parsed :: String -> [(Int,Int,Int)]
parsed str =  catMaybes $ tuple . fmap read . (splitOn ",") <$> lines str

tuple :: [Int] -> Maybe Point
tuple (a:b:c:_) = Just (a,b,c)
tuple _ = Nothing

tupleInt :: (Char,Char,Char) -> (Int,Int,Int)
tupleInt (a,b,c) = (digitToInt a, digitToInt b, digitToInt c)

part1 :: [(Int,Int,Int)] -> Int
part1 xyzs = flip execState 0 $ for_ xyzs (step s)
  where s = S.fromList xyzs

step :: MonadState Int m => S.Set Point -> Point -> m ()
step s p = do
  let numNeighbours = length (filter (flip S.member s) (neighbours p))
  modify $ (+) (6 - numNeighbours)

neighbours :: (Num a, Num b, Num c) => (a, b, c) -> [(a, b, c)]
neighbours (x, y, z) =  [ (x + dx, y + dy, z + dz) | (dx, dy, dz) <- [(0,0,1), (0,0,-1),
                                                                      (0,1,0), (0,-1,0),
                                                                      (1,0,0), (-1,0,0)] ]

-- Shock outside our identified [0..max] range to ensure the floodfill reaches everywhere
neighbours3d :: (Ord a, Ord b, Ord c, Num a, Num b, Num c) => (a, b, c) -> (a, b, c) -> [(a, b, c)]
neighbours3d (maxX, maxY, maxZ) (x, y, z) =  [ (x', y', z') | (dx, dy, dz) <- [(0,0,1), (0,0,-1),
                                                                               (0,1,0), (0,-1,0),
                                                                               (1,0,0), (-1,0,0)],
                                               let (x', y', z') = (x + dx,y + dy,z + dz),
                                               x' >= -1, y' >= -1, z' >= -1,
                                               x' <= maxX + 1, y' <= maxY + 1, z' <= maxZ +1]
x_ :: (Int, Int, Int) -> Int
x_ (a,b,c) = a

y_ :: (Int, Int, Int) -> Int
y_ (a,b,c) = b

z_ :: (Int, Int, Int) -> Int
z_ (a,b,c) = c

part2 :: [(Int,Int,Int)] -> Int
part2 xyzs = flip evalState (S.fromList $ fullGrid m) $ part2' xyzs
  where
    m = (max x_, max y_, max z_)
    max fn = maximum $ map fn xyzs

(<&&>) :: ((Int, Int, Int) -> Bool) -> ((Int, Int, Int) -> Bool) -> (Int, Int, Int) -> Bool
(<&&>) = liftA2 (&&)

fullGrid :: Point -> [Point]
fullGrid (xMax, yMax, zMax) = [(x,y,z) | x <- [-1 .. xMax + 1], y <- [-1 .. yMax + 1],z <- [-1 .. zMax + 1]]

type Unvisited = S.Set Point

-- Take start, and bounding cube coordinate set, visited
-- calculate U/D-L/R-F/B neighbours.
-- If any of the neighbours are in the reference list of nodes, stop searching
-- Once traversed everything we can reach, the remaining nodes are the air bubbles
-- If any of these air bubbles are in the reference list, then we can safely ignore them
-- For the remaining cubes, check neighbours that are in the reference list and subtract these.
-- These are the inner facing edges.
part2' :: [(Int,Int,Int)] -> State Unvisited Int
part2' xyzs = do
  floodFill m ref [(-1,-1,-1)]
  airPockets <- get
  pure $ part1 xyzs - (length $ filter (`S.member` ref) $ concatMap neighbours $ S.toList (airPockets S.\\ ref))
  where
    ref = S.fromList xyzs
    m = (max x_, max y_, max z_)
    max fn = maximum $ map fn xyzs

-- | Pass a reference list of points, that if we hit, we stop searching.
-- So they act kind of like a wall. Keep proceeding with floodfill
-- until there are no more neighbours to process
floodFill :: Point -> S.Set Point -> [Point] -> State Unvisited ()
floodFill maxes ref [] = pure ()
floodFill maxes ref (now:rest) = do
  unvisited <- get
  modify $ S.delete now
  let neighbours =
        if S.notMember now ref
        then filter (flip S.member unvisited <&&> (not . flip elem rest)) (neighbours3d maxes now)
        else mempty in
    floodFill maxes ref (rest ++ neighbours)

-- Testing data load first and last record
propLoadedDataValid :: Bool
propLoadedDataValid = parsed example !! 0 == (2,2,2) && parsed example !! 12 == (2,3,5)

-- Invariant: surrounding points should always be 2 + 2 + 2
propNeighbours :: Point -> Bool
propNeighbours cube = length (neighbours cube) == 6

-- Solution is the same irrespective of the order of the array
propReverse :: Bool
propReverse = part1 (parsed example) == part1 (reverse $ parsed example)

--A single cube surface area is invariant
propSingle :: Point -> Bool
propSingle (x,y,z) = part1 [(x,y,z)] == 6
