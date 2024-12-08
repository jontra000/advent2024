module P8 (run1, run2, inputLocation) where
import Lib (textToCoordMap, addCoords)
import Data.List (nub)
import qualified Data.Map as M

run1 = solve1 . parse

run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input8"

parse = textToCoordMap

solve1 input = length $ nub $ concatMap (antinodes input) $ antennaTypes input

antennaTypes = nub . filter (/='.') . M.elems 

antinodes input antennaType =
    let antennaLocs = M.keys $ M.filter (==antennaType) input
        antinodes = concatMap (projectAntinode antennaLocs) antennaLocs
    in  filter (`M.member` input) antinodes

projectAntinode otherAntennas antenna = map go (filter (/= antenna) otherAntennas)
    where go antenna' =
            let delta = subtractCoords antenna' antenna
            in  addCoords delta antenna'

subtractCoords (x1,y1) (x2,y2) = (x1-x2, y1-y2)

solve2 input = length $ nub $ concatMap (antinodes2 input) $ antennaTypes input

antinodes2 input antennaType =
    let antennaLocs = M.keys $ M.filter (==antennaType) input
    in  concatMap (projectAntinodes input antennaLocs) antennaLocs

projectAntinodes input otherAntennas antenna = concatMap go (filter (/= antenna) otherAntennas)
    where go antenna' =
            let delta = subtractCoords antenna' antenna
            in  takeWhile (`M.member` input) (iterate (addCoords delta) antenna')
