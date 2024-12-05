module P4SubMatrix (run1, run2, inputLocation) where
import Data.List (transpose)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input4"

parse :: String -> [String]
parse = lines

solve1 :: [String] -> Int
solve1 = sum . map xmasCount . subMatrices 4

subMatrices :: Int -> [String] -> [[String]]
subMatrices n = concatMap (transpose . map (divvy n 1)) . divvy n 1

divvy :: Int -> Int -> [a] -> [[a]]
divvy _ _ [] = []
divvy n m xs = take n xs : divvy n m (drop m xs)

xmasCount :: [String] -> Int
xmasCount subMatrix = length $ filter isXmas $ map (\f -> f subMatrix) [head, map (!! 0), diagonalDown, diagonalUp]

diagonalDown :: [String] -> String
diagonalDown [] = ""
diagonalDown xs
    | length xs == length (head xs) = zipWith (!!) xs [0..]
    | otherwise = ""

diagonalUp :: [String] -> String
diagonalUp [] = ""
diagonalUp xs
    | length xs == length (head xs) = zipWith (!!) xs [length xs - 1, length xs - 2..0]
    | otherwise = ""

isXmas :: String -> Bool
isXmas "XMAS" = True
isXmas "SAMX" = True
isXmas _ = False

solve2 :: [String] -> Int
solve2 = length . filter isMasX . subMatrices 3

isMasX :: [String] -> Bool
isMasX [['M',_,'M'], [_,'A',_], ['S',_,'S']] = True
isMasX [['M',_,'S'], [_,'A',_], ['M',_,'S']] = True
isMasX [['S',_,'S'], [_,'A',_], ['M',_,'M']] = True
isMasX [['S',_,'M'], [_,'A',_], ['S',_,'M']] = True
isMasX _ = False