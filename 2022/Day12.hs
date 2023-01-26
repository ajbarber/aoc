import Prelude

import Test.QuickCheck
import Control.Applicative ((<|>), liftA2)
import Control.Arrow ((&&&))
import Data.Vector qualified as V
import Data.Char
import Data.List
import Data.Function
import qualified Data.Ix as IX
import Data.Maybe
import Data.Tuple
import Debug.Trace
import qualified Data.Sequence as SQ
import qualified Data.Set as ST

example = "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi\n"

type Grid a = V.Vector (V.Vector a)

type Point = (Int, Int)

data Path a = Path a (Path a) | Nil deriving (Show, Functor)
data Value a =  Elevation a | Start | End deriving (Eq, Ord, Show)

main :: IO ()
main = do
  let parsed = parse example
  print $ val (2, 5) parsed
  print $ parsed -- rows
  print $ V.length (parsed V.! 0) -- cols
  putStrLn $ show $ bfs parsed (SQ.singleton start) ST.empty

parse :: String -> Grid (Value Int)
parse str = fmap (fmap toElevation) . V.fromList $ V.fromList <$> lines str
  where
    toElevation c = case c of
      'S' -> Start
      'E' -> End
      z -> Elevation (ord z)

instance Functor Value where
  fmap f Start = Start
  fmap f (Elevation a) = Elevation (f a)
  fmap f End = End

(<&&>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<&&>) = liftA2 (&&)

(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) = liftA2 (||)

maybeZip :: [a] -> (a -> Maybe b) -> [(a,b)]
maybeZip xs f = foldl' (\b a -> case f a of
                          Just x -> (a,x):b
                          Nothing -> b) [] xs

neighbours :: Point -> Grid (Value Int) -> ST.Set Point -> [Point]
neighbours loc@(m,n) grid visited =  case (val loc grid) of
  Just now -> fmap fst . filter (((>=) (succ <$> now) . snd) <||> last' now) $ maybeZip xs $ flip val grid
  Nothing -> []
  where
    last' n = (const (n == Elevation 122) <&&> (==) End) . snd
    xs = filter (flip ST.notMember visited) [ adjacent | (dx,dy) <- [(0,1), (-1,0), (1,0), (0,-1)],
                                              let adjacent = (m + dy, n + dx)]

val :: Point -> Grid (Value Int) -> Maybe (Value Int)
val (m,n) g = g V.!? m >>= flip (V.!?) n

start :: Path (Point,Int)
start = Path ((0,0),0) Nil

-- bfs
-- take a grid, queue
-- check if queue is empty
-- if empty then return accumulated result (here just an Int, path length)
-- else pop queue and visit that node
-- add this nodes' neighbours to queue and recurse
bfs :: Grid (Value Int) -> SQ.Seq (Path (Point, Int)) -> ST.Set Point -> [Path (Point,Int)]
bfs grid SQ.Empty _ = []
bfs grid q@(h@(Path (v,pt) prev) SQ.:<| rest) visited = let path = if val v grid == Just End then (:) h else id in
  path $ bfs grid (rest SQ.>< SQ.fromList (fmap (flip Path h) (,pt+1) <$> neighbours v grid visited)) (ST.insert v visited)

--- Quick check testing

testGrid :: Grid (Value Int)
testGrid = V.fromList $ V.fromList <$> [ [Start, Elevation 124, Elevation 123],
                                         [Elevation 121, Elevation 120, Elevation 122],
                                         [Elevation 120, Elevation 119, End],
                                         [Elevation 118, Elevation 117, Elevation 5]]

prop_grid :: Bool
prop_grid =  let f@([Path (p,s) _]) = bfs testGrid (SQ.singleton start) ST.empty in trace (show f) s == 4
