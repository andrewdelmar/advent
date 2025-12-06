import Data.List
import Data.List.Split

strOp "+" = (+)
strOp "*" = (*)

main = do
  input <- readFile "input.txt"
  let ops = map strOp $ words $ last $ lines input
  let nums = transpose $ map (map read . words) $ init $ lines input
  let totals = zipWith foldl1 ops nums
  print $ sum totals
