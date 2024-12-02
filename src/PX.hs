module PX (run1, run2, inputLocation) where

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/inputX"

parse = map parseLine . lines

parseLine = map (const "") . words

solve1 = const 0

solve2 = const 0