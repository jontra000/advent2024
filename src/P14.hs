module P14 (run1, run2, inputLocation) where
import Data.List.Split (splitOn)
import Lib (Coord, addCoords)
import qualified Data.Set as S

run1 = solve1 . parse

run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input14"

parse = map parseLine . lines

parseLine = parseWords . words

parseWords (posS:velS:_) = (parseCoord posS, parseCoord velS)

parseCoord = parseCoord' . splitOn "," . drop 2

parseCoord' :: [String] -> Coord
parseCoord' (x:y:_) = (read x, read y)

solve1 = safetyFactor . elapseTime 100

elapseTime t = map (elapseTime' t) 

elapseTime' t (start, velocity) = wrapCoords $ addCoords start (mulCoord t velocity)

mulCoord n (x,y) = (n*x,n*y)

wrapCoords (x,y) = (x `mod` 101, y `mod` 103)

safetyFactor state = product $ map (quadrantCount state) quadrants

quadrantCount state quadrantPredicate = length $ filter quadrantPredicate state

quadrants = [\(x,y) -> x < 50 && y < 51, \(x,y) -> x > 50 && y < 51, \(x,y) -> x < 50 && y > 51, \(x,y) -> x > 50 && y > 51]

solve2 :: [(Coord, Coord)] -> Int
solve2 = fst . head . filter (isTree . snd) . zip [0..] . map (S.fromList . map fst) . iterate step

step :: [(Coord, Coord)] -> [(Coord, Coord)]
step = map step'

step' (start, velocity) = (wrapCoords (addCoords start velocity), velocity)

isTree :: S.Set Coord -> Bool
isTree state = all (\c -> S.member c state) treeCoords

treeCoords = map (\x -> (x, 28)) [28..58]