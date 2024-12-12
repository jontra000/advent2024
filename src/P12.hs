module P12 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Lib (Coord, textToCoordMap, Direction (..), subtractCoords)
import Data.List (groupBy, sortOn, sort)

data Edge = Edge {direction :: Direction, level :: Int, loc :: Int} deriving Show

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input12"

parse :: String -> M.Map Coord Char
parse = textToCoordMap

solve1 :: M.Map Coord Char -> Int
solve1 = sum . map cost . regions

regions :: M.Map Coord Char -> [[Coord]]
regions m = case M.toList m of
                [] -> []
                ((c,x):_) ->
                    let (region, m') = makeRegion x ([], m) c
                    in  region : regions m'

makeRegion :: Char -> ([Coord], M.Map Coord Char) -> Coord -> ([Coord], M.Map Coord Char)
makeRegion x (region, m) c
    | M.lookup c m == Just x =
        let neighbours' = neighbours c
            m' = M.delete c m
            region' = c : region
        in  foldl (makeRegion x) (region', m') neighbours'
    | otherwise = (region, m)

neighbours :: Coord -> [Coord]
neighbours (x,y) = [(x+1,y),(x-1,y),(x,y-1),(x,y+1)]

cost :: [Coord] -> Int
cost region = area * perimeter region
    where area = length region

perimeter :: [Coord] -> Int
perimeter region = sum $ map (sideLength (S.fromList region)) region

sideLength :: S.Set Coord -> Coord -> Int
sideLength region c = length $ filter (`S.notMember` region) (neighbours c)

solve2 :: M.Map Coord Char -> Int
solve2 =  sum . map cost2 . regions

cost2 :: [Coord] -> Int
cost2 region = area * sides region
    where area = length region

sides :: [Coord] -> Int
sides = countSides . edges

edges :: [Coord] -> [Edge]
edges region = concatMap (edges' (S.fromList region)) region

edges' :: S.Set Coord -> (Int, Int) -> [Edge]
edges' region c =
    let xs = filter (`S.notMember` region) (neighbours c)
    in  map (edge c) xs

edge :: Coord -> Coord -> Edge
edge from@(x,y) to
    | d == (1,0) = Edge DirLeft x y
    | d == (-1,0) = Edge DirRight x y
    | d == (0,1) = Edge DirDown y x
    | d == (0,-1) = Edge DirUp y x
    | otherwise = error "bad neighbour"
    where d = subtractCoords to from

countSides :: [Edge] -> Int
countSides xs = sum $ map countSidesDir $ groupBy (\a b -> direction a == direction b) $ sortOn direction xs

countSidesDir :: [Edge] -> Int
countSidesDir xs = sum $ map (countSidesLoc . map loc) $ groupBy (\a b -> level a == level b) $ sortOn level xs

countSidesLoc :: [Int] -> Int
countSidesLoc = countContiguous . sort

countContiguous :: [Int] -> Int
countContiguous [] = 0
countContiguous (x:xs) = 1 + go x xs
    where go _ [] = 0
          go x' (y:ys)
            | y == x' + 1 = go y ys
            | otherwise = 1 + go y ys