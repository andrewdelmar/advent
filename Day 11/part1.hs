import Data.Map qualified as Map
import Data.Set qualified as Set

parseNode line =
  let (nstr : conns) = words line
      name = init nstr
   in (name, conns)

numPaths seen nodes target node
  | node == target = 1
  | node `Set.member` seen = 0
  | otherwise =
      let nextNode = nodes Map.! node
          nextSeen = Set.insert node seen
          nextPaths = map (numPaths nextSeen nodes target) nextNode
       in sum nextPaths

main = do
  input <- readFile "input.txt"
  let nodes = Map.fromList $ map parseNode $ lines input
  print $ numPaths Set.empty nodes "out" "you"
