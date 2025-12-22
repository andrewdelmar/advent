import Data.List.Split (splitOn)

parsePresent (_ : ls) = ls

data Region = Region Int Int [Int]
  deriving (Show)

presentArea = sum . map (length . filter (== '#'))

parseRegion str =
  let (size : pcounts) = words str
      [x, y] = map read $ splitOn ['x'] $ init size
   in Region x y (map read pcounts)

regionArea (Region x y _) = x * y

presentAreaNeeded presents (Region _ _ pcounts) =
  let pareas = map presentArea presents
   in sum (zipWith (*) pcounts pareas)

main = do
  input <- readFile "input.txt"
  let parts = splitOn [""] $ lines input
  let presents = map parsePresent $ init parts
  let regions = map parseRegion $ last parts
  let areas = map regionArea regions
  let areasNeeded = map (presentAreaNeeded presents) regions
  -- Assume that you can always arrange presents if there is enough area for them.
  -- How silly.
  let enough = zipWith (<=) areasNeeded areas
  print $ sum $ map fromEnum enough
