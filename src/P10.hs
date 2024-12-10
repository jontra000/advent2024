module P10 (run1, run2, inputLocation) where
import Lib (textToCoordMap, Coord)
import Data.Char (digitToInt)
import qualified Data.Map as M
import Data.List (nub)

run1 = solve1 . parse

run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input10"

parse = M.map digitToInt . textToCoordMap

solve1 m = sum $ map (score m) $ trailheads m

trailheads = M.keys . M.filter (==0)

score m = length . nub . reachableNines m 0

reachableNines :: M.Map Coord Int -> Int -> Coord -> [Coord]
reachableNines _ 9 loc = [loc]
reachableNines m currentVal loc =
    let nextVal = currentVal + 1
        reachableNeighbours = filter (\c -> M.lookup c m == Just nextVal) (neighbours loc)
    in  concatMap (reachableNines m nextVal) reachableNeighbours

neighbours (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

solve2 m = sum $ map (rating m) $ trailheads m

rating m = length . reachableNines m 0