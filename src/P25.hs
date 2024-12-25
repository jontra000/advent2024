module P25 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Data.List (transpose)

data Item = Key [Int] | Lock [Int]

run1 = solve1 . parse

run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input25"

parse = map parseItem . splitOn [""] . lines

parseItem xs =
    if head (head xs) == '#'
    then Lock (map (length . takeWhile (== '#')) (transpose xs))
    else Key (map (length . takeWhile ( == '#') . reverse) (transpose xs))

parseLine = map (const "") . words

solve1 items = 
    let keys = filter isKey items
        locks = filter isLock items
    in  sum $ map (unlocks locks) keys

isKey (Key _) = True
isKey _ = False

isLock (Lock _) = True
isLock _ = False

unlocks locks key = length $ filter (canUnlock key) locks

canUnlock (Key key) (Lock lock) = all (<=7) $ zipWith (+) key lock

solve2 = const 0