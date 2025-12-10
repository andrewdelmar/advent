import Data.List

parseCoord line =
  (read a, read $ tail b)
  where
    (a, b) = break (== ',') line

squareSize ((ax, ay), (bx, by)) =
  (abs (ax - bx) + 1) * (abs (ay - by) + 1)

main = do
  input <- readFile "input.txt"
  let coords :: [(Int, Int)] = map parseCoord $ lines input
  let squares = [(a, b) | a <- coords, b <- coords, a > b]
  print $ maximum $ map squareSize squares
