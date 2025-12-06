import Data.List.Split

parseRange input =
  [(read a :: Int) .. (read b :: Int)]
  where
    (a, _ : b) = break (== '-') input

repeatedAt s n =
  all (== c) cs
  where
    (c : cs) = chunksOf n s

repeated i =
  any (repeatedAt s) [1 .. (length s `div` 2)]
  where
    s = show i

main = do
  input <- readFile "input.txt"
  let ranges = map parseRange $ splitOn "," input
  let sums = map (sum . filter repeated) ranges
  print $ sum sums
