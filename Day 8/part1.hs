import Data.Graph qualified as Graph
import Data.List
import Data.List.Split
import Data.Set qualified as Set

type Coord = (Int, Int, Int)

parseCoord line =
  let (x : y : z : _) = map read $ splitOn "," line
   in (x, y, z)

data Connection = Connection {a :: Coord, b :: Coord}
  deriving (Show)

instance Eq Connection where
  (==) (Connection la lb) (Connection ra rb) =
    (la, lb) == (ra, rb) || (lb, la) == (ra, rb)

instance Ord Connection where
  compare a b = compare (conDist a) (conDist b)

conDist (Connection (ax, ay, az) (bx, by, bz)) =
  let (dx, dy, dz) = (ax - bx, ay - by, az - bz)
   in dx * dx + dy * dy + dz * dz

conContains coord (Connection a b) =
  coord == a || coord == b

otherCoord coord (Connection a b) =
  if coord == a then b else a

edgesOf cons coord =
  let icons = Set.filter (conContains coord) cons
   in map (otherCoord coord) $ Set.elems icons

main = do
  input <- readFile "input.txt"
  let coords :: [(Int, Int, Int)] = map parseCoord $ lines input
  let cons = Set.fromList [Connection a b | a <- coords, b <- coords, a /= b]

  let appliedCons = Set.take 1000 cons
  
  let edges = zip3 coords coords $ map (edgesOf appliedCons) coords
  let (graph, _, _) = Graph.graphFromEdges edges
  let forest = sortBy (flip compare) $ map length $ Graph.dff graph
  print $ product $ take 3 forest
