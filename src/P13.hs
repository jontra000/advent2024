module P13 (run1, run2, inputLocation) where
import Data.List.Split (splitOn)
import Lib (subtractCoords)
import Data.Maybe (mapMaybe)

run1 = solve1 . parse

run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input13"

parse = map parseItem . splitOn [""] . lines

parseItem (a:b:prize:_) = (parseButton (words a), parseButton (words b), parsePrize (words prize))

parseButton s = (parseCoord (init (s !! 2)), parseCoord (s !! 3))

parseCoord :: String -> Int
parseCoord = read . drop 2

parsePrize s = (parseCoord (init (s !! 1)), parseCoord (s !! 2))

solve1 = sum . map fewestTokensToWin

fewestTokensToWin machine = safeMin $ mapMaybe (cost machine) [0..100]

cost (a@(xa, ya), b@(xb, yb), prize@(xp, yp)) aPresses =
    let target = subtractCoords prize (xa * aPresses, ya * aPresses)
    in  case dropWhile (isBefore target . mulCoord b) [0..100] of
            [] -> Nothing
            (c:_) | (mulCoord b c) == target -> Just (c + 3*aPresses)
            _ -> Nothing

safeMin [] = 0
safeMin xs = minimum xs

mulCoord (x,y) n = (n*x,n*y)

isBefore (x1,y1) (x2,y2) = x2 < x1 && y2 <y1

solve2 = sum . map fewestTokensToWin2 . map shiftPrize

shiftPrize (a, b, (x,y)) = (a, b, (x+10000000000000,y+10000000000000))

fewestTokensToWin2 ((xa, ya), (xb, yb), (xt, yt)) =
    let denB = xt * ya - yt * xa
        numB = xb * ya - xa * yb
        denA = xt * yb - yt * xb
        numA = xa * yb - xb * ya
    in  if denB `mod` numB == 0 && denA `mod` numA == 0
        then denB `div` numB + 3 * denA `div` numA
        else 0