import Data.List
import Data.List.Split
import Data.Maybe

data Machine = Machine {target :: [Bool], masks :: [[Bool]]}

parseTarget = map (== '#')

parseMask size str =
  map (`elem` inds) [0 .. (size - 1)]
  where
    inds = map read $ splitOn "," str

parseMachine line =
  let (ls : ms) = words line
      trim = tail . init
      target = parseTarget $ trim ls
      size = length target
      masks = map (parseMask size . trim) (init ms)
   in Machine target masks

combos [] = [[]]
combos (m : ms) = combos ms ++ [m : cs | cs <- combos ms]

bestValidCombo (Machine target masks) =
  let xor a b = a /= b
      comboXor = foldl1 (zipWith xor)
      valid combo = all not $ comboXor (target : combo)
   in fromJust $ find valid $ combos masks

main = do
  input <- readFile "input.txt"
  let machines = map parseMachine $ lines input
  let bestValid = map bestValidCombo machines
  let comboSize = map length bestValid
  print $ sum comboSize
