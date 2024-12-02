import Data.Foldable

safeInc a b = a < b && b - a >= 1 && b - a <= 3

allSafeInc (a : b : vs) = safeInc a b && allSafeInc (b : vs)
allSafeInc _ = True

allSafeWithSkip prev (a : b : vs) =
  if safeInc a b
    then allSafeWithSkip (Just a) (b : vs)
    else allSafeInc (toList prev ++ (b : vs)) || allSafeInc (a : vs)
allSafeWithSkip _ _ = True

safeReport line = allSafeWithSkip Nothing vals || allSafeWithSkip Nothing (reverse vals)
  where
    vals = map read (words line)

main = do
  file <- readFile "input.txt"
  let reports = lines file
  let safeReports = filter safeReport reports
  print (length safeReports)
