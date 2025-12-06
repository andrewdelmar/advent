import Data.Char

nextMax first vals =
  maximum (tail (dropWhile (/= first) vals))

main = do
  input <- readFile "input.txt"
  let batts = map (map digitToInt) (lines input)
  let firsts = map (maximum . init) batts
  let seconds = zipWith nextMax firsts batts
  let vals = zipWith (\a b -> a * 10 + b) firsts seconds
  print $ sum vals
