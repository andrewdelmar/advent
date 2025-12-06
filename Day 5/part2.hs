import Data.List

parseSegment line =
  [(read ss, 1), (read $ tail es, -1)]
  where
    (ss, es) = break (== '-') line

edgeSeg ((seg, ddir), depth) =
  (ddir == -1 && depth == 0) || (ddir == 1 && depth == 1)

total (s : e : ts) = e - s + 1 + total ts
total [] = 0

main = do
  input <- readFile "input.txt"
  let (fresh, _) = break (== "") (lines input)
  let segs = sort $ concatMap parseSegment fresh
  let depths = tail $ scanl (+) 0 $ map snd segs
  let edges = map (fst . fst) $ filter edgeSeg $ zip segs depths

  print $ total edges
