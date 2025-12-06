import Data.Char

maxN 1 vals = maximum vals
maxN n vals =
  let first = maximum $ reverse $ drop (n - 1) $ reverse vals
      after = tail $ dropWhile (/= first) vals
   in first * 10 ^ (n - 1) + maxN (n - 1) after

main = do
  input <- readFile "input.txt"
  let batts = map (map digitToInt) (lines input)
  print $ sum $ map (maxN 12) batts
