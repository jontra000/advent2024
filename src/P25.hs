module P25 (run1, run2, inputLocation) where

import Data.List.Split (splitOn)
import Data.List (transpose)

data Item = Key [Int] | Lock [Int]

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Integer
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input25"

parse :: String -> [Item]
parse = map parseItem . splitOn [""] . lines

parseItem :: [String] -> Item
parseItem xs =
    if head (head xs) == '#'
    then Lock (map (length . takeWhile (== '#')) (transpose xs))
    else Key (map (length . takeWhile ( == '#') . reverse) (transpose xs))

solve1 :: [Item] -> Int
solve1 items = length $ filter id $ [canUnlock a b | a <- items, b <- items]

canUnlock :: Item -> Item -> Bool
canUnlock (Key key) (Lock lock) = all (<=7) $ zipWith (+) key lock
canUnlock _ _ = False

solve2 :: b -> Integer
solve2 = const 0