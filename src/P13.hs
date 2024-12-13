module P13 (run1, run2, inputLocation) where
import Data.List.Split (splitOn)
import Lib (Coord)

data Machine = Machine Coord Coord Coord -- button a, button b, target

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input13"

parse :: String -> [Machine]
parse = map parseMachine . splitOn [""] . lines

parseMachine :: [String] -> Machine
parseMachine (a:b:prize:_) = Machine (parseButton (words a)) (parseButton (words b)) (parsePrize (words prize))
parseMachine _ = error "Bad input"

parseButton :: [String] -> Coord
parseButton s = (parseCoord (init (s !! 2)), parseCoord (s !! 3))

parseCoord :: String -> Int
parseCoord = read . drop 2

parsePrize :: [String] -> Coord
parsePrize s = (parseCoord (init (s !! 1)), parseCoord (s !! 2))

solve1 :: [Machine] -> Int
solve1 = sum . map tokenCost

solve2 :: [Machine] -> Int
solve2 = solve1 . map shiftPrize

shiftPrize :: Machine -> Machine
shiftPrize (Machine a b (x,y)) = Machine a b (x+10000000000000,y+10000000000000)

tokenCost :: Machine -> Int
tokenCost (Machine (xa, ya) (xb, yb) (xt, yt)) =
    let denB = xt * ya - yt * xa
        numB = xb * ya - xa * yb
        denA = xt * yb - yt * xb
        numA = xa * yb - xb * ya
    in  if denB `mod` numB == 0 && denA `mod` numA == 0
        then denB `div` numB + 3 * denA `div` numA
        else 0