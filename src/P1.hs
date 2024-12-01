module P1 (run1, run2, inputLocation) where

import Data.List (sort, transpose)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input1"

parse :: String -> [[Int]]
parse = transpose . map parseLine . lines

parseLine :: String -> [Int]
parseLine = map read . words

solve1 :: [[Int]] -> Int
solve1 (input1:input2:_) = sum $ map abs $ zipWith (-) (sort input1) (sort input2)

solve2 :: [[Int]] -> Int
solve2 (input1:input2:_) = sum $ map (similarityScore input2) input1

similarityScore :: [Int] -> Int -> Int
similarityScore list2 x = x * length (filter (==x) list2)