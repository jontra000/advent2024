module P6 (run1, run2, inputLocation) where
import Lib (textToCoordMap, Coord, Direction(..), move, rotateRight)
import Data.List (nub, find)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S

type MapGrid = M.Map Coord Char
type State = (Direction, Coord)
data Input = Input MapGrid State

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input6"

parse :: String -> Input
parse = initialState . textToCoordMap

solve1 :: Input -> Int
solve1 = length . nub . map snd . walk

initialState :: MapGrid -> Input
initialState initMap = Input initMap (DirUp, startLocation initMap)

startLocation :: MapGrid -> Coord
startLocation = fst . fromJust . find ((=='^') . snd) . M.toList

walk :: Input -> [State]
walk (Input grid start) = walk' start
    where walk' state@(dir, loc) =
            let loc' = move dir loc
            in  case M.lookup loc' grid of
                    Just '#' -> walk' (rotateRight dir, loc)
                    Nothing -> [state]
                    _ -> state : walk' (dir, loc')

solve2 :: Input -> Int
solve2 input = length $ filter (causesLoop input) potentialLocs
    where potentialLocs = nub $ map snd $ tail $ walk input

causesLoop :: Input -> Coord -> Bool
causesLoop (Input grid start) obstruction = checkLoop S.empty $ walk (Input (M.insert obstruction '#' grid) start)

checkLoop :: S.Set State -> [State] -> Bool
checkLoop _ [] = False
checkLoop prevStates (x:xs)
    | S.member x prevStates = True
    | otherwise = checkLoop (S.insert x prevStates) xs