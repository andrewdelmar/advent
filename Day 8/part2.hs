import Data.List
import Data.List.Split
import Data.Set qualified as Set

type Coord = (Int, Int, Int)

parseCoord line =
  let (x : y : z : _) = map read $ splitOn "," line
   in (x, y, z)

data Connection = Connection {a :: Coord, b :: Coord}

instance Eq Connection where
  (==) (Connection la lb) (Connection ra rb) =
    (la, lb) == (ra, rb) || (lb, la) == (ra, rb)

instance Ord Connection where
  compare a b = compare (conDist a) (conDist b)

conDist (Connection (ax, ay, az) (bx, by, bz)) =
  let (dx, dy, dz) = (ax - bx, ay - by, az - bz)
   in dx * dx + dy * dy + dz * dz

cordLength (ax, _, _) (bx, _, _) = ax * bx

addCoords (seen, lastXMul) (Connection a b) =
  (Set.insert a $ Set.insert b seen, cordLength a b)

main = do
  input <- readFile "input.txt"
  let coords :: [(Int, Int, Int)] = map parseCoord $ lines input
  let cons = Set.fromList [Connection a b | a <- coords, b <- coords, a /= b]

  let coordsSeen = scanl addCoords (Set.empty, 0) $ Set.elems cons
  let numCoords = length coords
  print $ snd <$> find ((==) numCoords . Set.size . fst) coordsSeen
