import Data.List
import Data.Ord qualified as Ord

parseCoord line =
  (read a, read $ tail b)
  where
    (a, b) = break (== ',') line

overlap (al, ah) (bl, bh)
  | bl <= al && bh >= al = True
  | bl <= ah && bh >= ah = True
  | bl >= al && bh <= ah = True
  | otherwise = False

order (a, b)
  | a < b = (a, b)
  | otherwise = (b, a)

xbounds (a, b) = order (fst a, fst b)

ybounds (a, b) = order (snd a, snd b)

xsize s = xh - xl + 1
  where
    (xl, xh) = xbounds s

ysize s = yh - yl + 1
  where
    (yl, yh) = ybounds s

size s = xsize s * ysize s

squareOverlap a b =
  let xoverlap = overlap (xbounds a) (xbounds b)
      yoverlap = overlap (ybounds a) (ybounds b)
   in xoverlap && yoverlap

noOverlap poly s =
  not (any (squareOverlap (innerSquare s)) poly)

innerSquare s =
  let (xl, xh) = xbounds s
      (yl, yh) = ybounds s
   in ((xl + 1, yl + 1), (xh - 1, yh - 1))

main = do
  input <- readFile "input.txt"
  let coords :: [(Int, Int)] = map parseCoord $ lines input
  let poly = zip coords (tail $ cycle coords)
  let squares = [(a, b) | a <- coords, b <- coords, a > b]

  -- Dumb Assumption 1: No square with a size <= 2 units can be the largest.
  let bigEnough = filter ((> 2) . xsize) $ filter ((> 2) . ysize) squares
  let bigFirst = sortOn (Ord.Down . size) bigEnough

  -- Dumb Assumption 2: No segment of the polygon is directly adjacent to another.
  -- A segment crossing a square indicates some area must be external to the polygon.
  let biggest = find (noOverlap poly) bigFirst
  print $ size <$> biggest
