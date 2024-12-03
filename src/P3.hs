module P3 (run1, run2, inputLocation) where
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

run1 :: String -> Int
run1 = solve1 . parse1

run2 :: String -> Int
run2 = solve2 . parse2

inputLocation :: String
inputLocation = "inputs/input3"

parse1 :: [Char] -> [String]
parse1 = splitOn "mul"

solve1 :: [String] -> Int
solve1 = sum . map parseMultiplication

parseMultiplication :: String -> Int
parseMultiplication ('(':xs)
    | ')' `elem` xs = tryMultiply $ parseArguments xs
    | otherwise = 0
parseMultiplication _ = 0

parseArguments :: String -> [Maybe Int]
parseArguments = map readMaybe . splitOn "," . takeWhile (/=')')

tryMultiply :: [Maybe Int] -> Int
tryMultiply [Just a, Just b] = a * b
tryMultiply _ = 0

parse2 :: String -> [String]
parse2 = splitOn "don't()"

solve2 :: [String] -> Int
solve2 [] = 0
solve2 (x:xs) = run1 x + sum (map parseDontBlock xs)

parseDontBlock :: String -> Int
parseDontBlock = run1 . parseDos

parseDos :: String -> String
parseDos = concat . safeTail . splitOn "do()"

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs