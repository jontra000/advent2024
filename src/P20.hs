module P20 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Lib (textToCoordMap)
import qualified Dijkstra20 as D20

run1 = solve1 . parse

run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input20"

parse = textToCoordMap

solve1 x = findCheats 2 100 <$> bestPath x

bestPath m = snd <$> D20.dijkstra (adjGraph m) (start m) [(end m)]

adjGraph m =
    let freeNodes = M.keysSet $ M.filter (/= '#') m
    in  M.fromList $ map (\n -> (n, adjNodes freeNodes n)) $ S.toList freeNodes

adjNodes m n = M.fromList $ map (\n' -> (n', 1)) $ filter (`S.member` m) $ adjCoords n

adjCoords (x, y) = [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]

start m = head $ M.keys $ M.filter (== 'S') m

end m = head $ M.keys $ M.filter (== 'E') m

findCheats _ _ [] = 0
findCheats cheatTime saved (x:xs) = length (filter (\(honest, cheat) -> cheat <= cheatTime && honest >= cheat + saved) $ zip [1..] $ map (distance x) xs) + findCheats cheatTime saved xs

solve2 x = findCheats 20 100 <$> bestPath x

distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)