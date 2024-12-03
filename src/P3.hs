module P3 (run1, run2, inputLocation) where
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Data.Char (isDigit)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2

inputLocation :: String
inputLocation = "inputs/input3"

parse = splitOn "mul"

solve1 :: [[Char]] -> Int
solve1 = sum . map parseMultiplication

parseMultiplication :: [Char] -> Int
parseMultiplication ('(':xs) 
    | ')' `elem` xs = tryMultiply $ map readMaybe $ splitOn "," $ takeWhile (/=')') xs
    | otherwise = 0
parseMultiplication _ = 0

tryMultiply :: [Maybe Int] -> Int
tryMultiply (Just a: Just b:[]) = a * b
tryMultiply _ = 0

solve2 ('m':'u':'l':'(':xs) =
    let (a, xs') = span isDigit xs
    in  case xs' of
        (',':xs'') ->
            let (b, xs''') = span isDigit xs''
            in  case xs''' of
                (')':xs'''') -> (read a * read b) + solve2 xs''''
                _ -> solve2 xs'''
        _ -> solve2 xs'
solve2 ('d':'o':'n':'\'':'t':'(':')':xs) = reenable xs
solve2 [] = 0
solve2 (_:xs) = solve2 xs

reenable ('d':'o':'(':')':xs) = solve2 xs
reenable [] = 0
reenable (_:xs) = reenable xs