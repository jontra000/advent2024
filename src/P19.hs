module P19 (run1, run2, inputLocation) where

import Data.List.Split (splitOn)
import Data.List (isPrefixOf)
import Data.MemoTrie (memo)

data Input = Input [String] [String]

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input19"

parse :: String -> Input
parse = parseTowels . lines

parseTowels :: [String] -> Input
parseTowels (available:_:targets) = Input (splitOn ", " available) targets
parseTowels e = error ("Invalid input: " ++ show (unlines e))

solve1 :: Input -> Int
solve1 (Input available targets) = length $ filter (canMake available) targets

canMake :: Eq a => [[a]] -> [a] -> Bool
canMake _ [] = True
canMake available target =
    let matches = filter (`isPrefixOf` target) available
    in  any (canMake available . (`drop` target) . length) matches

solve2 :: Input -> Int
solve2 (Input available targets) = sum $ map goMemo targets
    where goMemo = memo go 
          go [] = 1
          go target = 
            let matches = filter (`isPrefixOf` target) available
            in  sum $ map (goMemo . (`drop` target) . length) matches