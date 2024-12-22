module P22 (run1, run2, inputLocation) where

import qualified Data.Map as M
import Data.Bits
import Data.List.Split (divvy)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input22"

parse :: String -> [Int]
parse = map parseLine . lines

parseLine :: String -> Int
parseLine = read

solve1 :: [Int] -> Int
solve1 = sum . map ((!! 2000) . iterate nextSecret)

nextSecret :: Int -> Int
nextSecret = mixAndPrune (*2048) . mixAndPrune (`div` 32) . mixAndPrune (*64)

mixAndPrune :: (Int -> Int) -> Int -> Int
mixAndPrune f x =
    let x' = f x
        x'' = x .^. x'
    in  x'' `mod` 16777216

solve2 :: [Int] -> Int
solve2 = maximum . M.elems . priceForDeltaSeq

diffs :: [Int] -> [Int]
diffs xs = zipWith (-) (tail xs) xs

priceForDeltaSeq :: [Int] -> M.Map [Int] Int
priceForDeltaSeq = M.unionsWith (+) . map (priceForDeltaSeq' . prices)

prices :: Int -> [Int]
prices = map (`mod` 10) . take 2001 . iterate nextSecret

priceForDeltaSeq' :: [Int] -> M.Map [Int] Int
priceForDeltaSeq' xs = M.fromList $ reverse $ zip (triggerSeqs xs) (drop 4 xs)

triggerSeqs :: [Int] -> [[Int]]
triggerSeqs = divvy 4 1 . diffs