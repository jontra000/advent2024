module P9 (run1, run2, inputLocation) where
import Data.List.Split (chunksOf)
import Data.Char (digitToInt)
import qualified Data.Set as S

data Block = Block { index :: Int, full :: Int, empty :: Int }

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input9"

parse :: String -> [Block]
parse = zipWith makeBlock [0..] . chunksOf 2 . map digitToInt

makeBlock :: Int -> [Int] -> Block
makeBlock ix [full] = Block ix full 0
makeBlock ix [full, empty] = Block ix full empty
makeBlock _ _ = error "Invalid block"

solve1 :: [Block] -> Int
solve1 = checksum . compact1

compact1 :: [Block] -> [Int]
compact1 blocks = take totalBlocks $ writeBlocks blocks blocksRev
    where totalBlocks = sum $ map full blocks
          blocksRev = concatMap writeBlock $ reverse blocks

writeBlocks :: [Block] -> [Int] -> [Int]
writeBlocks [] _ = []
writeBlocks (Block ix full empty : xs) revXs = replicate full ix ++ toFill ++ writeBlocks xs revXs'
            where (toFill, revXs') = splitAt empty revXs

writeBlock :: Block -> [Int]
writeBlock (Block ix full _) = replicate full ix
    
checksum :: [Int] -> Int
checksum = sum . zipWith (*) [0..]

solve2 :: [Block] -> Int
solve2 = checksum . compact2

compact2 :: [Block] -> [Int]
compact2 = compact2' S.empty

compact2' :: S.Set Int -> [Block] -> [Int]
compact2' _ [] = []
compact2' movedBlocks ((Block ix full empty):xs) = replicate full value ++ fillEmpty movedBlocks xs empty (reverse xs)
        where value = if S.member ix movedBlocks then 0 else ix

fillEmpty :: S.Set Int -> [Block] -> Int -> [Block] -> [Int]
fillEmpty movedBlocks xs toFill [] = replicate toFill 0 ++ compact2' movedBlocks xs
fillEmpty movedBlocks xs toFill ((Block ix full _):xsRev)
    | toFill < full = fillEmpty movedBlocks xs toFill xsRev
    | S.member ix movedBlocks = fillEmpty movedBlocks xs toFill xsRev
    | otherwise = replicate full ix ++ fillEmpty movedBlocks' xs remainingSpace xsRev
        where movedBlocks' = S.insert ix movedBlocks
              remainingSpace = toFill - full