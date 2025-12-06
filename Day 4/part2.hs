import Data.Array
import Data.List
import Data.Maybe

neighbors (x, y) = delete (x, y) [(x + i, y + j) | i <- [-1 .. 1], j <- [-1 .. 1]]

inBounds arr (x, y) =
  x >= lx && x <= hx && y >= ly && y <= hy
  where
    ((lx, ly), (hx, hy)) = bounds arr

isRoll arr coord = inBounds arr coord && arr ! coord == '@'

numRollNeighbors arr coord =
  length $ filter id $ map (isRoll arr) (neighbors coord)

forkliftable arr coord =
  isRoll arr coord && numRollNeighbors arr coord < 4

liftIfPossible arr coord =
  if isRoll arr coord && not (forkliftable arr coord) then '@' else '.'

lifted arr = listArray (bounds arr) (map (liftIfPossible arr) (indices arr))

stable arr = numRolls arr == numRolls (lifted arr)

numRolls arr = length (filter (== '@') $ elems arr)

main = do
  input <- readFile "input.txt"
  let width = length $ head $ lines $ input
  let height = length $ lines $ input
  let oneline = foldl (++) "" $ transpose $ lines input
  let field = listArray ((1, 1), (width, height)) oneline
  let lifts = iterate lifted field
  let init = numRolls field
  let done = fromJust $ find stable lifts
  print (init - numRolls done)
