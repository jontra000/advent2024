module P7 (run1, run2, inputLocation) where

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input7"

parse :: String -> [(Int, [Int])]
parse = map (parseLine . words) . lines

parseLine :: [String] -> (Int, [Int])
parseLine (x:xs) = (read (init x), map read xs)

solve1 :: [(Int, [Int])] -> Int
solve1 = sum . map fst . filter hasSolution

hasSolution :: (Int, [Int]) -> Bool
hasSolution (result, terms) = any (==result) $ generateResults (reverse terms)

generateResults :: [Int] -> [Int]
generateResults [x] = [x]
generateResults [] = [0]
generateResults (x:xs) = map (x+) (generateResults xs) ++ map (*x) (generateResults xs)

solve2 :: [(Int, [Int])] -> Int
solve2 = sum . map fst . filter hasSolution2

hasSolution2 :: (Int, [Int]) -> Bool
hasSolution2 (result, (term1:terms)) = any (==result) $ generateResults2 term1 terms

generateResults2 :: Int -> [Int] -> [Int]
generateResults2 acc [] = [acc]
generateResults2 acc (x:xs) =
    let nextAcc = [acc+x, acc*x, read (show acc ++ show x)]
    in  concatMap (\acc' -> generateResults2 acc' xs) nextAcc

