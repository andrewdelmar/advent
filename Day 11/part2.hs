import Data.Map qualified as Map
import Data.Set qualified as Set

parseNode line =
  let (nstr : conns) = words line
      name = init nstr
      connVals = map (,1) conns
   in (name, Map.fromList connVals)

addConns = Map.unionWith (+)

mulConns conns a = Map.map (* a) conns

collapseNode nodes cNode =
  let cConns = nodes Map.! cNode
      updates = Map.mapWithKey (collapseInto cNode cConns) nodes
   in Map.delete cNode updates

collapseInto cNode cConns node conns
  | cNode `Map.member` conns =
      let mult = conns Map.! cNode
          newConns = conns `addConns` (cConns `mulConns` mult)
       in Map.filterKeys (`notElem` [node, cNode]) newConns
  | otherwise = conns

pathCardn nodes [_] = 1
pathCardn nodes (p : np : ps) =
  let conns = nodes Map.! p
      c = Map.findWithDefault 0 np conns
   in c * pathCardn nodes (np : ps)

main = do
  input <- readFile "input.txt"
  let nodes = Map.fromList $ map parseNode $ lines input
  let toCollapse = filter (`notElem` ["svr", "out", "dac", "fft"]) $ Map.keys nodes
  let cNodes = foldl collapseNode nodes toCollapse
  let totalPaths =
        pathCardn cNodes ["svr", "fft", "dac", "out"]
          + pathCardn cNodes ["svr", "dac", "fft", "out"]
  print totalPaths
