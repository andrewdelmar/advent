safeInc a b = a < b && b - a >= 1 && b - a <= 3

allSafeInc (a : b : vs) = safeInc a b && allSafeInc (b : vs)
allSafeInc _ = True

safeReport line = allSafeInc vals || allSafeInc (reverse vals)
  where
    vals = map read (words line)

main = do
  file <- readFile "input.txt"
  let reports = lines file
  let safeReports = filter safeReport reports
  print (length safeReports)
