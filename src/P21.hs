module P21 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (isDigit)
import Data.MemoTrie (memo2)

run1 = solve1 . parse

run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input21"

parse = lines

solve1 = sum . map complexity

solve2 = sum . map (complexityMemo 25)

complexityMemo n s = (numericPart s) *  shortestSequenceMemo n (numericInput s)

complexity x = (numericPart x) * length (shortestSequence x)

numericPart = read . takeWhile isDigit

shortestSequence = (!! 2) . iterate robotInput . numericInput
 
shortestSequenceMemo :: Int -> [Char] -> Int
shortestSequenceMemo = goMemo
    where goMemo = memo2 go
          go 0 s = length s
          go n s = sum $ map (goMemo (n-1)) paths
            where locs = (2,0) : map dirLoc s
                  paths = zipWith pathLoc locs (tail locs)

numericInput input = go (2,3) input
    where go _ [] = []
          go loc (x:xs) =
            let loc' = numericLoc x
            in  pathNumeric loc loc' ++ go loc' xs

robotInput input = concat $ zipWith pathLoc locs (tail locs)
    where locs = (2,0) : map dirLoc input

numericLoc '7' = (0,0)
numericLoc '8' = (1,0)
numericLoc '9' = (2,0)
numericLoc '4' = (0,1)
numericLoc '5' = (1,1)
numericLoc '6' = (2,1)
numericLoc '1' = (0,2)
numericLoc '2' = (1,2)
numericLoc '3' = (2,2)
numericLoc '0' = (1,3)
numericLoc 'A' = (2,3)
numericLoc e = error ("Invalid input: " ++ [e])

dirLoc '^' = (1,0)
dirLoc 'A' = (2,0)
dirLoc '<' = (0,1)
dirLoc 'v' = (1,1)
dirLoc '>' = (2,1)
dirLoc e = error ("Invalid input: " ++ [e])

pathNumeric (x1,y1) (x2,y2)
    | y1 == 3 && x2 == 0 = replicate (-dy) '^' ++ replicate (-dx) '<' ++ replicate dy 'v' ++ replicate dx '>' ++ "A"
    | y2 == 3 && x1 == 0 = replicate (-dx) '<' ++ replicate dx '>' ++ replicate dy 'v' ++ replicate (-dy) '^' ++ "A"
    | otherwise = replicate (-dx) '<' ++ replicate dy 'v' ++ replicate (-dy) '^' ++ replicate dx '>' ++ "A"
    where dx = x2 - x1
          dy = y2 - y1
          
pathLoc (x1,y1) (x2,y2)
    | x2 == 0 = replicate dy 'v' ++ replicate (-dx) '<' ++ replicate (-dy) '^' ++ replicate dx '>' ++ "A"
    | x1 == 0 = replicate (-dx) '<' ++ replicate dx '>' ++ replicate dy 'v' ++ replicate (-dy) '^' ++ "A"
    | otherwise = replicate (-dx) '<' ++ replicate dy 'v' ++ replicate (-dy) '^' ++ replicate dx '>' ++ "A"
    where dx = x2 - x1
          dy = y2 - y1