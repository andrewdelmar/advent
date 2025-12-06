import Data.Array
import Data.List

neighbors (x, y) = delete (x, y) [(x + i, y + j) | i <- [-1 .. 1], j <- [-1 .. 1]]

inBounds arr (x, y) =
  x >= lx && x <= hx && y >= ly && y <= hy
  where
    ((lx, ly), (hx, hy)) = bounds arr

isRoll arr coord =
  inBounds arr coord && arr ! coord == '@'

numRollNeighbors arr coord =
  length $ filter id $ map (isRoll arr) (neighbors coord)

forkliftable arr coord =
  isRoll arr coord && numRollNeighbors arr coord < 4

main = do
  input <- readFile "input.txt"
  let width = length $ head $ lines input
  let height = length $ lines input
  let oneline = concat $ transpose $ lines input
  let field = listArray ((1, 1), (width, height)) oneline

  let liftable = map (forkliftable field) (indices field)
  print $ length $ filter id liftable
