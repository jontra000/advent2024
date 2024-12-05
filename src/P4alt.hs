module P4alt (run1, run2, inputLocation) where
import Lib (Coord)
import Data.Maybe (catMaybes)
import Data.List (transpose, tails)
import qualified Data.Set as S

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input4"

parse :: String -> [String]
parse = lines

solve1 :: [String] -> Int
solve1 = sum . map xmasCount . transformInputs

transformInputs :: [[a]] -> [[[a]]]
transformInputs input = map (\f -> f input) [id, transpose, diagonal, diagonal . reverse]

diagonal :: [[a]] -> [[a]]
diagonal = map (reverse . catMaybes) . transpose . augInput

augInput :: [[a]] -> [[Maybe a]]
augInput = zipWith augRow [0..]

augRow :: Int -> [a] -> [Maybe a]
augRow i row = replicate i Nothing ++ map Just row

xmasCount :: [String] -> Int
xmasCount = sum . map xmasCount'

xmasCount' :: String -> Int
xmasCount' = length . filter (isXmas . take 4) . tails

isXmas :: String -> Bool
isXmas "XMAS" = True
isXmas "SAMX" = True
isXmas _ = False

solve2 :: [String] -> Int
solve2 input = length $ S.intersection masLocs1 masLocs2
    where rowCount = length input - 1
          diagInput1 = diagonal input
          diagInput2 = diagonal (reverse input)
          masLocs1 = S.fromList $ map (translateCoords rowCount) $ masLocations diagInput1
          masLocs2 = S.fromList $ map (translateReversedCoords rowCount) $ masLocations diagInput2

masLocations :: [String] -> [Coord]
masLocations = concatMap (\(colIndex, row) -> map (\rowIndex -> (rowIndex, colIndex)) $ masLocationsRow row) . zip [0..]

masLocationsRow :: String -> [Int]
masLocationsRow = map fst . filter (isMas . snd) . zip [1..] . map (take 3) . tails

isMas :: String -> Bool
isMas "MAS" = True
isMas "SAM" = True
isMas _ = False

translateCoords :: Int -> Coord -> Coord
translateCoords n (x,y) = (x + max 0 (y - n), min y n - x)

translateReversedCoords :: Int -> Coord -> Coord
translateReversedCoords n (x,y) = (x + max 0 (y - n), x + max 0 (n - y))