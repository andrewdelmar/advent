import Data.Foldable (find)
import Data.List.Split
import Data.Map qualified as Map
import Data.Maybe (fromJust)

type IMap = Map.Map Int Int

data Machine = Machine {target :: IMap, masks :: [IMap]}
  deriving (Show)

trim = tail . init

parseMasks str = Map.fromList kvs
  where
    vals :: [Int] = map read $ splitOn "," $ trim str
    kvs = map (,1) vals

parseTarget str = Map.fromList kvs
  where
    vals :: [Int] = map read $ splitOn "," $ trim str
    kvs = zip [0 ..] vals

parseMachine line =
  let (_ : parts) = words line
      target = parseTarget $ last parts
      masks = map parseMasks $ init parts
   in Machine target masks

data MaskCombo = MaskCombo {combiM :: IMap, lastM :: IMap, presses :: Int}
  deriving (Show)

combine (MaskCombo combiM _ presses) mask =
  MaskCombo (combiM `addUnion` mask) mask (presses + 1)

valid machine combo = combiM combo == target machine

tooHigh target maskCombo =
  any (< 0) result
  where
    result = target `addUnion` Map.map negate (combiM maskCombo)

nextIter (Machine target masks) prevCombos =
  [ combine maskCombo mask
    | maskCombo <- prevCombos,
      mask <- masks,
      mask <= lastM maskCombo,
      not (tooHigh target (combine maskCombo mask))
  ]

comboIters machine =
  concat $ iterate (nextIter machine) initial
  where
    initial = map (\m -> MaskCombo m m 1) (masks machine)

addUnion = Map.unionWith (+)

firstValid machine = find (valid machine) $ comboIters machine

main = do
  input <- readFile "input.txt"
  let machines = map parseMachine $ lines input
  mapM_ (print . firstValid) machines

-- let sizes = map minPresses machines
--
-- mapM_ print sizes
-- print $ sum sizes
