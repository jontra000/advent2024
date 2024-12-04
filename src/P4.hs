module P4 (run1, run2, inputLocation) where
import Lib (Coord)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input4"

parse = lines

solve1 input = sum $ map (xmasCount input) [(x,y) | x <- [0..length(head input)], y <- [0..length input]]

xmasCount input c = length $ filter (xmasInDir input c "XMAS") [(0,1), (1,0), (-1,0), (0,-1), (1,1), (-1,-1),(-1,1),(1,-1)]

xmasInDir :: [String] -> Coord -> String -> Coord -> Bool
xmasInDir _ _ [] _ = True
xmasInDir input (x,y) (c:xs) (dx, dy)
    | y < 0 = False
    | y >= length input = False
    | x < 0 = False
    | x >= length (input !! y) = False
    | (input !! y) !! x == c = xmasInDir input (x',y') xs (dx,dy)
    | otherwise = False
        where (x', y') = (x+dx, y+dy)

solve2 input = length $ filter (masCount input) [(x,y) | x <- [0..length(head input)-1], y <- [0..length input-1]]

masCount input c = charAt input c 'A' && masDiagonal input c

masDiagonal input c = (masInDir input c (1,1) || masInDir input c (-1,-1)) && (masInDir input c (1,-1) || masInDir input c (-1,1))

masInDir input (x,y) (dx, dy) = charAt input (x+dx,y+dy) 'M' && charAt input (x-dx,y-dy) 'S'

charAt input (x,y) c
    | y < 0 = False
    | y >= length input = False
    | x < 0 = False
    | x >= length (input !! y) = False
    | otherwise = (input !! y) !! x == c