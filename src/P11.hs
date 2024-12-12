module P11 (run1, run2, inputLocation) where

import qualified Data.Map as M
import Lib (memoize)

type Cache = M.Map (Int, Int) Int

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input11"

parse :: String -> [Int]
parse = map read . words

solve1 :: [Int] -> Int
solve1 = length . (!! 25) . iterate blink

blink :: [Int] -> [Int]
blink = concatMap blinkStone

blinkStone :: Int -> [Int]
blinkStone 0 = [1]
blinkStone x
    | even (length s) = splitStone s
    | otherwise = [x * 2024]
        where s = show x

splitStone :: String -> [Int]
splitStone = map read . tupleToList . splitInHalf

tupleToList :: (a, a) -> [a]
tupleToList (a, b) = [a, b]

splitInHalf :: [a] -> ([a], [a])
splitInHalf s = splitAt (length s `div` 2) s

solve2 :: [Int] -> Int
solve2 = fst . stoneCountAfterBlinks M.empty 75

blinkStoneCached :: (Int, Cache) -> (Int, Int) -> (Int, Cache)
blinkStoneCached (acc, cache) (0, _) = (acc + 1, cache)
blinkStoneCached (acc, cache) key@(i, x) = (acc + res', cache')
    where go = memoize cache key . stoneCountAfterBlinks cache (i-1) . blinkStone
          (res', cache') = go x

stoneCountAfterBlinks :: Cache -> Int -> [Int] -> (Int, Cache)
stoneCountAfterBlinks cache i = foldl blinkStoneCached (0, cache) . map (toTuple i)

toTuple :: a -> b -> (a, b)
toTuple x y = (x,y)
