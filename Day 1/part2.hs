parse ('L' : cs) = -(read cs)
parse ('R' : cs) = read cs

turn pos t = (pos + t) `mod` 100

passes pos t = abs ((pos + t) `quot` 100) + (if (pos + t) <= 0 && pos /= 0 then 1 else 0)

main = do
  input <- readFile "input.txt"
  let turns = map parse (lines input)
  let pos = scanl turn 50 turns
  let pass = zipWith passes pos turns
  print (sum pass)
