module P22 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bits
import Data.List (find, nub)
import Data.List.Split (divvy)

run1 = solve1 . parse

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

mixAndPrune f x =
    let x' = f x
        x'' = x .^. x'
    in  x'' `mod` 16777216

solve2 :: [Int] -> Int
solve2 secrets = maximum $ M.elems $ combineSaleMaps secrets'
    where secrets' = secretSeries secrets

diffs xs = zipWith (-) (tail xs) xs

secretSeries = map (secretSeries' . map (`mod` 10) . take 2001 . iterate nextSecret)

secretSeries' :: [Int] -> M.Map [Int] Int
secretSeries' xs =
    let subSeqs = divvy 4 1 (diffs xs)
    in  M.fromList $ reverse $ zip subSeqs (drop 4 xs)

combineSaleMaps :: [M.Map [Int] Int] -> M.Map [Int] Int
combineSaleMaps = M.unionsWith (+)