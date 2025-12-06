import Data.Char
import Data.List
import Data.List.Split

strOp "+" = (+)
strOp "*" = (*)

main = do
  input <- readFile "input.txt"
  let ops = map strOp $ words $ last $ lines input
  let cnums = splitWhen (all isSpace) $ transpose $ init $ lines input
  let nums = map (map read) cnums
  let totals = zipWith foldl1 ops nums
  print $ sum totals
