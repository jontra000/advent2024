module P5 (run1, run2, inputLocation) where
import Data.List.Split (splitOn)
import Data.List (delete)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input5"

parse :: String -> ([(Int, Int)], [[Int]])
parse = parse' . splitOn [""] . lines

parse' :: [[String]] -> ([(Int, Int)], [[Int]])
parse' (ruleBlock:printBlock:_) = (map (parseRule . splitOn "|") ruleBlock, map parsePrint printBlock)

parseRule :: [String] -> (Int, Int)
parseRule (a:b:_) = (read a, read b)

parsePrint :: String -> [Int]
parsePrint = map read . splitOn ","

solve1 :: ([(Int, Int)], [[Int]]) -> Int
solve1 (rules, printBlocks) = sum $ map middlePage $ filter (isValid rules) printBlocks

isValid :: [(Int, Int)] -> [Int] -> Bool
isValid rules printBlock = all (ruleApplies printBlock) rules

ruleApplies :: [Int] -> (Int, Int) -> Bool
ruleApplies printBlock (before,after) = after `notElem` printBlock || before `notElem` printBlock || before `elem` (takeWhile (/=after) printBlock)

middlePage :: [Int] -> Int
middlePage xs = xs !! ((length xs) `div` 2)

solve2 :: ([(Int, Int)], [[Int]]) -> Int
solve2 (rules, printBlocks) = sum $ map middlePage $ map (correctErrors rules) $ filter (not . isValid rules) printBlocks

correctErrors :: [(Int, Int)] -> [Int] -> [Int]
correctErrors _ [] = []
correctErrors rules block =
    let rules' = filter (\(a,b) -> a `elem` block && b `elem` block) rules
        next = head $ filter (\i -> all (\(_, after) -> after /= i) rules') block
    in  next : correctErrors rules' (delete next block)