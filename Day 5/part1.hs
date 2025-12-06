parseSegment line =
  (read ss, read $ tail es)
  where
    (ss, es) = break (== '-') line

inSeg t (s, e) = t >= s && t <= e

main = do
  input <- readFile "input.txt"
  let (fresh, rest) = break (== "") (lines input)
  let used :: [Int] = map read $ tail rest
  let segs = map parseSegment fresh
  let infresh = filter (\t -> any (inSeg t) segs) used
  print $ length infresh
