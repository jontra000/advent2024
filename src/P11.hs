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

splitInHalf :: [a] -> ([a], [a])
splitInHalf s = splitAt (length s `div` 2) s

tupleToList :: (a, a) -> [a]
tupleToList (a,b) = [a,b]

solve2 :: [Int] -> Int
solve2 = fst . foldl blinkStoneCached (0, M.empty) . map (toTuple 74)

blinkStoneCached :: (Int, Cache) -> (Int, Int) -> (Int, Cache)
blinkStoneCached (acc, cache) key@(i, x) = (acc + res', cache')
    where xs = map (toTuple (i-1)) (blinkStone x)
          (res', cache') = memoize cache key res
          res 
            | i == 0 = (length xs, cache)
            | otherwise = foldl blinkStoneCached (0, cache) xs

toTuple :: a -> b -> (a, b)
toTuple x y = (x,y)
