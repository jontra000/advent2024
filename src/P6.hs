module P6 (run1, run2, inputLocation) where
import Lib (textToCoordMap, Coord)
import Data.List (nub, find)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S

data Direction = DirUp | DirDown | DirLeft | DirRight deriving (Eq, Ord)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input6"

parse :: String -> M.Map Coord Char
parse = textToCoordMap

solve1 :: M.Map Coord Char -> Int
solve1 = length . nub . map snd . walk . initialState

initialState :: M.Map Coord Char -> (M.Map Coord Char, Direction, Coord)
initialState initMap = (initMap, DirUp, fst (fromJust (find ((=='^') . snd) (M.toList initMap))))

walk :: (M.Map Coord Char, Direction, Coord) -> [(Direction, Coord)]
walk (input, dir, loc) =
    let loc' = step dir loc
    in  case M.lookup loc' input of
            Just '#' -> walk (input, rotateRight dir, loc)
            Nothing -> [(dir, loc)]
            _ -> (dir, loc) : walk (input, dir, loc')

step :: Direction -> Coord -> Coord
step DirUp (x,y) = (x, y-1)
step DirDown (x,y) = (x,y+1)
step DirLeft (x,y) = (x-1,y)
step DirRight (x,y) = (x+1,y)

rotateRight :: Direction -> Direction
rotateRight DirUp = DirRight
rotateRight DirRight = DirDown
rotateRight DirDown = DirLeft
rotateRight DirLeft = DirUp

solve2 :: M.Map Coord Char -> Int
solve2 input = length $ filter (causesLoop initState) potentialLocs
    where initState = initialState input
          potentialLocs = nub $ map snd $ tail $ walk initState

causesLoop :: (M.Map Coord Char, Direction, Coord) -> Coord -> Bool
causesLoop (grid, dir, loc) obstruction = checkLoop S.empty $ walk (M.insert obstruction '#' grid, dir, loc)

checkLoop :: S.Set (Direction, Coord) -> [(Direction, Coord)] -> Bool
checkLoop _ [] = False
checkLoop prevStates (x:xs)
    | S.member x prevStates = True
    | otherwise = checkLoop (S.insert x prevStates) xs