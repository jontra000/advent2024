module P8 (run1, run2, inputLocation) where
import Lib (textToCoordMap, addCoords, subtractCoords, Coord)
import Data.List (nub)
import qualified Data.Map as M
import qualified Data.Set as S

data Input = Input (S.Set Coord) [[Coord]]

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input8"

parse :: String -> Input
parse = mapToInput . textToCoordMap

mapToInput :: M.Map Coord Char -> Input
mapToInput inputMap = Input (M.keysSet inputMap) (antennaGroups inputMap)

antennaGroups :: M.Map Coord Char -> [[Coord]]
antennaGroups inputMap = map (keysForValue inputMap) (antennaTypes inputMap)

keysForValue :: Eq a => M.Map k a -> a -> [k]
keysForValue map' = M.keys . \a -> M.filter (==a) map'

antennaTypes :: M.Map k Char -> [Char]
antennaTypes = nub . filter (/='.') . M.elems 

solve1 :: Input -> Int
solve1 (Input mapCoords antennas) = solve (take 1 . tail) mapCoords antennas

solve2 :: Input -> Int
solve2 (Input mapCoords antennas) = solve id mapCoords antennas

solve :: ([Coord] -> [Coord]) -> S.Set Coord -> [[Coord]] -> Int
solve f mapCoords = length . nub . concatMap (antinodes f mapCoords)

antinodes :: ([Coord] -> [Coord]) -> S.Set Coord -> [Coord] -> [Coord]
antinodes f mapCoords antennas = concatMap (projectAntinodes f mapCoords antennas) antennas

projectAntinodes :: ([Coord] -> [Coord]) -> S.Set Coord -> [Coord] -> Coord -> [Coord]
projectAntinodes f mapCoords antennas antenna = concatMap go antennas'
    where antennas' = filter (/= antenna) antennas
          inBounds = (`S.member` mapCoords)
          go antenna' =
            let delta = subtractCoords antenna' antenna
                antinodes' = iterate (addCoords delta) antenna'
            in  takeWhile inBounds (f antinodes')
