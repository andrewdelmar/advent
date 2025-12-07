import Data.IntSet qualified as Set
import Data.List

splitBeams (beams, total) splitters =
  let hits = beams `Set.intersection` splitters
      numHits = Set.size hits
      misses = beams `Set.difference` splitters
      splits = Set.map (+ 1) hits `Set.union` Set.map (subtract 1) hits
      newBeams = splits `Set.union` misses
   in (newBeams, total + numHits)

fromElemInds e = Set.fromList . elemIndices e

main = do
  input <- readFile "input.txt"
  let rowlen = length $ head $ lines input
  let start = fromElemInds 'S' $ head $ lines input
  let splitters = map (fromElemInds '^') $ tail $ lines input
  let (_, total) = foldl splitBeams (start, 0) splitters
  print total
