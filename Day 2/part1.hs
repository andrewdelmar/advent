import Data.List.Split

parseRange input =
  [(read a :: Int) .. (read b :: Int)]
  where
    (a, _ : b) = break (== '-') input

repeated i =
  a == b
  where
    s = show i
    (a, b) = splitAt (length s `div` 2) s

main = do
  input <- readFile "input.txt"
  let ranges = map parseRange $ splitOn "," input
  let sums = map (sum . filter repeated) ranges
  print $ sum sums
