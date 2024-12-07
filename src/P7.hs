module P7 (run1, run2, inputLocation) where

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
concatenation a b = read $ show a ++ show b

hasSolution :: [Int -> Int -> Int] -> Input -> Bool
hasSolution operations (Input result (term1:terms)) = elem result $ generateResults operations term1 terms
hasSolution _ (Input _ []) = False

generateResults :: [Int -> Int -> Int] -> Int -> [Int] -> [Int]
generateResults _ acc [] = [acc]
generateResults operations acc (x:xs) = concatMap go operations
    where go op = generateResults operations acc' xs
            where acc' = op acc x
