module P2 (run1, run2, inputLocation) where
import Data.List (nub)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input2"

parse :: String -> [[Int]]
parse = map parseLine . lines

parseLine :: String -> [Int]
parseLine = map read . words

solve1 :: [[Int]] -> Int
solve1 = length . filter isSafe

isSafe :: [Int] -> Bool
isSafe input = isMonotonic diffs && differsByAtMost3 diffs
    where diffs = zipWith (-) input $ tail input

isMonotonic :: [Int] -> Bool
isMonotonic = allEqual . map signum

allEqual :: [Int] -> Bool
allEqual = (<= 1) . length . nub

differsByAtMost3 :: [Int] -> Bool
differsByAtMost3 = all ((<= 3) . abs)

solve2 :: [[Int]] -> Int
solve2 = length . filter isAlmostSafe

isAlmostSafe :: [Int] -> Bool
isAlmostSafe = any isSafe . safeVariants

safeVariants :: [a] -> [[a]]
safeVariants input = input : map (removeNth input) [0..length input]

removeNth ::[a] ->  Int -> [a]
removeNth xs n = take n xs ++ drop (n + 1) xs