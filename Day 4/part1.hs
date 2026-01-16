import Data.Array
import Data.List

neighbors (x, y) arr =
  [ (i, j)
    | i <- [x - 1 .. x + 1],
      j <- [y - 1 .. y + 1],
      (i, j) /= (x, y),
      i <= hx && i >= lx,
      j <= hy && j >= ly
  ]
  where
    ((lx, ly), (hx, hy)) = bounds arr

isRoll arr coord =
  arr ! coord == '@'

numRollNeighbors arr coord =
  length $ filter id $ map (isRoll arr) (neighbors coord arr)

forkliftable arr coord =
  isRoll arr coord && numRollNeighbors arr coord < 4

main = do
  input <- readFile "input.txt"
  let ls = lines input
  let width = length $ head ls
  let height = length ls
  let field = listArray ((1, 1), (width, height)) $ concat ls

  let liftable = length $ filter (forkliftable field) (indices field)
  print liftable
