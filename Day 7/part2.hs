import Data.IntMap qualified as Map
import Data.List

addUnion = Map.unionWith (+)

splitBeams beams splitters =
  let hits = beams `Map.intersection` splitters
      misses = beams `Map.difference` splitters
      splits = Map.mapKeys (+ 1) hits `addUnion` Map.mapKeys (subtract 1) hits
      newBeams = splits `addUnion` misses
   in newBeams

fromElemInds e = Map.fromList . map (,1) . elemIndices e

main = do
  input <- readFile "input.txt"
  let rowlen = length $ head $ lines input
  let start = fromElemInds 'S' $ head $ lines input
  let splitters = map (fromElemInds '^') $ tail $ lines input
  let end = foldl splitBeams start splitters
  print (Map.foldl (+) 0 end)
