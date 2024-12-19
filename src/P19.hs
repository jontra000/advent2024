module P19 (run1, run2, inputLocation) where

import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.List (isPrefixOf)
import Lib (memoize)

data Input = Input [String] [String]
type Cache = M.Map String Int

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
solve2 (Input available targets) = solveMemo available targets

solveMemo :: [String] -> [String] -> Int
solveMemo available targets = fst $ foldl go (0, M.empty) targets
    where go (acc, cache) target =
            let (result, cache') = solveSingle available cache target
            in  (acc + result, cache')

solveSingle :: [String] -> Cache -> String -> (Int, Cache)
solveSingle _ cache [] = (1, cache)
solveSingle available cache target = memoize cache target $ solveSingle' available cache target

solveSingle' :: [String] -> Cache -> [Char] -> (Int, Cache)
solveSingle' available cache target =
    let matches = filter (`isPrefixOf` target) available
        (result, cacheFinal) = foldl go (0, cache) matches
    in  (result, M.insert target result cacheFinal)
    where go (acc, cache') match =
            let (result, cache'') = solveSingle available cache' (drop (length match) target)
            in  (acc + result, cache'')