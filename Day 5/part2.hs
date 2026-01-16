import Data.List

parseSegment line =
  [read ss, read es]
  where
    (ss, '-' : es) = break (== '-') line

edgeSeg dir depth =
  (dir == -1 && depth == 0) || (dir == 1 && depth == 1)

total (s : e : ts) = e - s + 1 + total ts
total [] = 0

main = do
  input <- readFile "input.txt"
  let (fresh, _) = break (== "") (lines input)
  let segs :: [Int] = concatMap parseSegment fresh
  let (ssegs, dirs) = unzip $ sort $ zip segs $ cycle [1, -1]
  let depths = scanl1 (+) dirs
  let edges = zipWith edgeSeg dirs depths
  let edgeSegs = map fst $ filter snd $ zip ssegs edges

  print $ total edgeSegs
