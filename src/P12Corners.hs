module P12Corners (run1, run2, inputLocation) where

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
    | M.lookup c m == Just x = foldl (makeRegion x) (region', m') neighbours'
    | otherwise = (region, m)
        where neighbours' = neighbours c
              m' = M.delete c m
              region' = c : region

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
sides region = sum $ map cornerCount' region
    where cornerCount' = cornerCount (S.fromList region)

cornerCount :: S.Set Coord -> Coord -> Int
cornerCount region = length . filter (isCorner region) . corners

corners :: Coord -> [[Coord]]
corners (x,y) = [[(x-1, y), (x-1, y-1), (x, y-1)], [(x, y-1), (x+1, y-1), (x+1, y)], [(x+1, y), (x+1, y+1), (x, y+1)], [(x, y+1), (x-1, y+1), (x-1, y)]]

isCorner :: S.Set Coord -> [Coord] -> Bool
isCorner region cs = case map (`S.member` region) cs of
    [True, False, True] -> True
    [False, _, False] -> True
    _ -> False