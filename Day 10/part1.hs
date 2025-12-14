import Data.Bits
import Data.List
import Data.List.Split
import Data.Maybe

appendBit bits True = (bits .<<. 1) .|. 1
appendBit bits False = bits .<<. 1

fromBitPoss = foldl (.|.) (0 :: Int) . map bit

trim = tail . init

parseMask str =
  let poss = map read $ splitOn "," $ trim str
   in fromBitPoss poss

data Machine = Machine {numBits :: Int, target :: Int, masks :: [Int]}

parseMachine line =
  let (ls : ms) = words line
      lightStr = trim ls
      lightPoss = elemIndices '#' lightStr
      target = fromBitPoss lightPoss
      masks = map parseMask (init ms)
   in Machine (length lightStr) target masks

combineMasks masks bits =
  let useMask = map (testBit bits) [0 .. length masks - 1]
      usedMasks = map snd $ filter fst $ zip useMask masks
   in foldl xor 0 usedMasks

bestMaskCombo machine =
  let nmbits = length (masks machine)
      cbits = sortOn popCount [0 :: Int .. (1 .<<. nmbits) - 1]
      validCombo bits = combineMasks (masks machine) bits == target machine
      best = find validCombo cbits
   in fromJust best

main = do
  input <- readFile "input.txt"
  let machines = map parseMachine (lines input)
  let bestCombos = map bestMaskCombo machines
  print $ sum $ map popCount bestCombos
