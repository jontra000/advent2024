module P1 (run1, run2, inputLocation) where

import Data.List (sort, transpose)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input1"

parse :: String -> ([Int], [Int])
parse = take2 . transpose . map parseLine . lines
    where take2 (leftList:rightList:_) = (leftList, rightList)
          take2 _ = error "expected two columns of input"

parseLine :: String -> [Int]
parseLine = map read . words

solve1 :: ([Int], [Int]) -> Int
solve1 (leftList, rightList) = sum $ map abs $ zipWith (-) (sort leftList) (sort rightList)

solve2 :: ([Int], [Int]) -> Int
solve2 (leftList, rightList) = sum $ map (similarityScore rightList) leftList

similarityScore :: [Int] -> Int -> Int
similarityScore xs x = x * length (filter (==x) xs)