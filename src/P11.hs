module P11 (run1, run2, inputLocation) where

import qualified Data.Map as M
import Lib (memoize)

run1 = solve1 . parse

run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input11"

parse = map read . words

solve1 = length . (!! 25) . iterate blink

blink = concatMap blinkStone

blinkStone :: Int -> [Int]
blinkStone 0 = [1]
blinkStone x
    | even (length s) = map read $ tupleToList (splitAt (length s `div` 2) s)
    | otherwise = [x * 2024]
        where s = show x

tupleToList (a,b) = [a,b]

solve2 = fst . foldl blinkStone2 (0, M.empty) . map (\x -> (74, x))

blinkStone2 :: (Int, M.Map (Int, Int) Int) -> (Int, Int) -> (Int, M.Map (Int, Int) Int)
blinkStone2 (acc, cache) (i, x) = case M.lookup (i, x) cache of
        Just x' -> (acc + x', cache)
        Nothing ->
            let xs = map (\x' -> (i-1,x')) (blinkStone x)
                (res, cache') = if i == 0 then (length xs, cache) else foldl blinkStone2 (0, cache) xs
                cache'' = M.insert (i, x) res cache'
            in  (acc + res, cache'')
