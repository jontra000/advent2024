module P7 (run1, run2, inputLocation) where

import Data.NumberLength.Int (lengthInt)

data Input = Input { target :: Int, terms :: [Int] }

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input7"

parse :: String -> [Input]
parse = map (parseLine . words) . lines

parseLine :: [String] -> Input
parseLine (x:xs) = Input (read (init x)) (map read xs)
parseLine [] = error "Empty line not expected"

solve1 :: [Input] -> Int
solve1 = solve [(+), (*)]

solve2 :: [Input] -> Int
solve2 = solve [(+), (*), concatenation]

solve :: [Int -> Int -> Int] -> [Input] -> Int
solve operations = sum . map target . filter (hasSolution operations)

concatenation :: Int -> Int -> Int
concatenation a b = a * (10 ^ lengthInt b) + b

hasSolution :: [Int -> Int -> Int] -> Input -> Bool
hasSolution operations (Input target (term1:terms)) = hasSolution' target operations term1 terms
hasSolution _ (Input _ []) = False

hasSolution' :: Int -> [Int -> Int -> Int] -> Int -> [Int] -> Bool
hasSolution' target _ acc [] = acc == target
hasSolution' target operations acc (x:xs) 
    | acc > target = False
    | otherwise = any go operations
        where go op = hasSolution' target operations acc' xs
                where acc' = op acc x