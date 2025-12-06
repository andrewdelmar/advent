parse ('L' : cs) = -(read cs)
parse ('R' : cs) = read cs

turn pos t = (pos + t) `mod` 100

main = do
  input <- readFile "input.txt"
  let turns = map parse (lines input)
  let pos = scanl turn 50 turns
  let zeros = filter (== 0) pos
  print (length zeros)
