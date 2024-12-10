module P10 (run1, run2, inputLocation) where
import Lib (textToCoordMap, Coord)
import Data.Char (digitToInt)
import qualified Data.Map as M
import Data.List (nub)

type Input = M.Map Coord Int

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input10"

parse :: String -> Input
parse = M.map digitToInt . textToCoordMap

solve1 :: Input -> Int
solve1 m = solve (score m) m

trailheads :: Input -> [Coord]
trailheads = M.keys . M.filter (==0)

score :: Input -> Coord -> Int
score m = length . nub . reachableNines m 0

reachableNines :: Input -> Int -> Coord -> [Coord]
reachableNines _ 9 loc = [loc]
reachableNines m currentVal loc = concatMap (reachableNines m nextVal) reachableNeighbours
    where nextVal = currentVal + 1
          reachableNeighbours = nextSteps m nextVal loc

nextSteps :: Input -> Int -> Coord -> [Coord]
nextSteps m val = filter (valueExistsAt m val) . neighbours

valueExistsAt :: Input -> Int -> Coord -> Bool
valueExistsAt m val c = M.lookup c m == Just val

neighbours :: Coord -> [Coord]
neighbours (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

solve2 :: Input -> Int
solve2 m = solve (rating m) m

rating :: Input -> Coord -> Int
rating m = length . reachableNines m 0

solve :: (Coord -> Int) -> Input -> Int
solve f = sum . map f . trailheads