module P2 (run1, run2, inputLocation) where
import Data.List (nub)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input2"

parse = map parseLine . lines

parseLine :: String -> [Int]
parseLine = map read . words

solve1 = length . filter isSafe

isSafe input = isMonotonic input && differsByAtMost3 input

isMonotonic input = allEqual $ map signum $ zipWith (-) input $ tail input

allEqual = (<= 1) . length . nub

differsByAtMost3 :: [Int] -> Bool
differsByAtMost3 input = all ((<= 3) . abs) $ zipWith (-) input $ tail input

solve2 = length . filter isAlmostSafe

isAlmostSafe = any isSafe . safeVariants

safeVariants input = [input] ++ map (\n -> take n input ++ drop (n+1) input) [0..length input]