module P20 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Lib (textToCoordMap, Coord)
import Data.List (find)

type Map = M.Map Coord Char

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input20"

parse :: String -> [Coord]
parse s = path (M.keysSet $ M.filter (/= '#') m) (startNode m)
    where m = textToCoordMap s

solve1 :: [Coord] -> Int
solve1 = findCheats 2 100

adjCoords :: Coord -> [Coord]
adjCoords (x, y) = [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]

startNode :: Map -> Coord
startNode = head . M.keys . M.filter (== 'S')

findCheats :: Int -> Int -> [Coord] -> Int
findCheats _ _ [] = 0
findCheats cheatTime toSave (x:xs) = findCheatsFrom cheatTime toSave x xs + findCheats cheatTime toSave xs

findCheatsFrom :: Int -> Int -> Coord -> [Coord] -> Int
findCheatsFrom cheatTime toSave start = length . filter (>= toSave) . zipWith (cheatTimeSaved cheatTime start) [1..]

cheatTimeSaved :: Int -> Coord -> Int -> Coord -> Int
cheatTimeSaved maxCheat start honestDistance end
    | cheatTime <= maxCheat = honestDistance - cheatTime
    | otherwise = 0
    where cheatTime = distance start end

solve2 :: [Coord] -> Int
solve2 = findCheats 20 100

distance :: Coord -> Coord -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

path :: S.Set Coord -> Coord -> [Coord]
path m loc = case find (`S.member` m) (adjCoords loc) of
    Nothing -> [loc]
    Just next -> loc : path (S.delete loc m) next