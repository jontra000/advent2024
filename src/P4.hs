module P4 (run1, run2, inputLocation) where
import Lib (Coord, addCoords)
import Data.Maybe (mapMaybe)
import Data.List (sort)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input4"

parse :: String -> [String]
parse = lines

coords :: [String] -> [Coord]
coords input = [(x,y) | x <- [0..length (head input)-1], y <- [0..length input-1]]

solve1 :: [String] -> Int
solve1 input = sum $ map (xmasCount input) $ coords input

xmasCount :: [String] -> Coord -> Int
xmasCount input c = length $ filter (xmasInDir input c "XMAS") [(0,1), (1,0), (-1,0), (0,-1), (1,1), (-1,-1), (-1,1), (1,-1)]

xmasInDir :: [String] -> Coord -> String -> Coord -> Bool
xmasInDir _ _ [] _ = True
xmasInDir input coord@(x,y) (c:xs) delta = isCharAt input (x,y) c && xmasInDir input coord' xs delta
    where coord' = addCoords coord delta

solve2 :: [String] -> Int
solve2 input = length $ filter (isCrossMas input) $ coords input

isCrossMas :: [String] -> Coord -> Bool
isCrossMas input c = isCharAt input c 'A' && masDiagonal input c

masDiagonal :: [String] -> Coord -> Bool
masDiagonal input c = checkMas input c [(1,1), (-1,-1)] && checkMas input c [(-1,1), (1,-1)]

checkMas :: [String] -> Coord -> [Coord] -> Bool
checkMas input c deltas = sort (mapMaybe (charAt input . addCoords c) deltas) == "MS"

isCharAt :: [String] -> Coord -> Char -> Bool
isCharAt input coord c = charAt input coord == Just c

charAt :: [String] -> Coord -> Maybe Char
charAt input (x,y)
    | y < 0 = Nothing
    | y >= length input = Nothing
    | x < 0 = Nothing
    | x >= length (input !! y) = Nothing
    | otherwise = Just $ (input !! y) !! x