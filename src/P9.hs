module P9 (run1, run2, inputLocation) where
import Data.List.Split (chunksOf)
import Data.Char (digitToInt)

run1 = solve1 . parse

run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input9"

parse :: String -> [Int]
parse = map digitToInt

-- solve1 :: [Int] -> Integer
solve1 = checksum . chunksOf 2

-- checksum :: [[Int]] -> Integer
checksum xs = sum $ zipWith (*) [0..] $ checksum' xs' (reverse xs')
    where xs' = zip [0..] xs

checksum' :: [(Int, [Int])] -> [(Int, [Int])] -> [Int]
checksum' [] _ = []
checksum' _ [] = []
checksum' ((i, (full:empty:_)):xs) ((j, (full':_)):xsrev)
    | j <= i = replicate full' i
    | empty == full' = replicate full i ++ replicate empty j ++ checksum' xs xsrev
    | empty < full' = replicate full i ++ replicate empty j ++ checksum' xs ((j, [full' - empty]):xsrev)
    | otherwise = replicate full i ++ replicate full' j ++ if j > i+1 then checksum' ((i, [0, empty - full']):xs) xsrev else []

solve2 = checksum2 . chunksOf 2

checksum2 = checksum2' . moveBlocks . makeDisk 0

makeDisk _ [] = []
makeDisk i ((full:empty:_):xs) = (i, full, empty) : makeDisk (i+1) xs
makeDisk i ([full]:xs) = (i, full, 0) : makeDisk (i+1) xs

moveBlocks xs = foldl moveBlocks' xs (reverse xs)

moveBlocks' [] _ = []
moveBlocks' (x@(j, full', empty'):xs) next@(i, full, _)
    | j == i = x : xs
    | full > empty' = x : moveBlocks' xs next
    | otherwise = (j, full', 0) : dropMovedBlock i (i, full, empty' - full) xs

dropMovedBlock i prev@(prevI, prevFull, prevEmpty) (next@(i', full, empty):xs)
    | i == i' = (prevI, prevFull, prevEmpty + full + empty) : xs
    | otherwise = prev : dropMovedBlock i next xs
dropMovedBlock _ _ [] = []

checksum2' = sum . zipWith (*) [0..] . concatMap toIds

toIds (i, full, empty) = replicate full i ++ replicate empty 0