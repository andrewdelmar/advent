import Data.List.Split
import Data.Maybe

data Machine = Machine {target :: [Int], masks :: [[Bool]]}

parseTarget = map read . splitOn ","

parseMask size str =
  map (`elem` inds) [0 .. (size - 1)]
  where
    inds = map read $ splitOn "," str

parseMachine line =
  let (_ : ms) = words line
      trim = tail . init
      target = parseTarget $ trim $ last ms
      size = length target
      masks = map (parseMask size . trim) (init ms)
   in Machine target masks

combos [] = [[]]
combos (m : ms) = combos ms ++ [m : cs | cs <- combos ms]

validCombos ptarget masks =
  let allCombos = combos masks
      xor a b = a /= b
      comboXor = foldl1 (zipWith xor)
      valid combo = all not $ comboXor (ptarget : combo)
   in filter valid allCombos

maybeNonEmpty list =
  if null list then Nothing else Just list

subCombo target combo =
  let cvals = map (map fromEnum) combo
   in foldl (zipWith (-)) target cvals

pressesNeeded m@(Machine target masks)
  | any (< 0) target = Nothing
  | all (== 0) target = Just 0
  | otherwise =
      do
        let parity = map odd target
        vcombos <- maybeNonEmpty $ validCombos parity masks
        presses <- maybeNonEmpty $ mapMaybe (pressesWithCombo m) vcombos
        Just (minimum presses)

pressesWithCombo (Machine target masks) combo =
  do
    let presses = length combo
    let newTarget = map (`div` 2) $ subCombo target combo
    subPresses <- pressesNeeded (Machine newTarget masks)
    Just (presses + subPresses * 2)

main = do
  input <- readFile "input.txt"
  let machines = map parseMachine $ lines input
  let presses = map (fromJust . pressesNeeded) machines

  print $ sum presses