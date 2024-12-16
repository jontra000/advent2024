module P15Set (run1, run2, inputLocation) where
import Data.List.Split (splitOn)
import Lib (Coord, textToCoordMap, Direction (..))
import qualified Data.Map as M
import qualified Data.Set as S

data State = State Coord (S.Set Coord) (S.Set Coord) deriving Show -- robot walls boxes

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input15"

parse :: String -> (State, [Direction])
parse = parseBlock . splitOn [""] . lines

parseBlock :: [[String]] -> (State, [Direction])
parseBlock (mapInput:moveInput:_) = (parseMap mapInput, parseMoves moveInput)
parseBlock _ = error "Bad input"

parseMap :: [String] -> State
parseMap input = State robot walls boxes
    where m = textToCoordMap $ unlines input
          walls = M.keysSet $ M.filter (=='#') m
          boxes = M.keysSet $ M.filter (=='O') m
          robot = head $ M.keys $ M.filter (=='@') m

expandMap :: State -> State
expandMap (State robot walls boxes) = State (expandCoord robot) (S.unions $ S.map expandWall walls) (S.map expandCoord boxes)

expandCoord :: Coord -> Coord
expandCoord (x,y) = (2*x, y)

expandWall :: Coord -> S.Set Coord
expandWall (x,y) = S.fromList [(2*x, y), (2*x+1, y)]

parseMoves :: [String] -> [Direction]
parseMoves = map parseMove . concat

parseMove :: Char -> Direction
parseMove '>' = DirRight
parseMove '<' = DirLeft
parseMove 'v' = DirDown
parseMove '^' = DirUp
parseMove c = error ("Bad move char: " ++ [c])

solve1 :: (State, [Direction]) -> Int
solve1 (state, moves) = gpsSum $ foldl move state moves

move :: State -> Direction -> State
move state@(State loc walls boxes) dir =
    let moves = tail $ iterate (step dir) loc
        res = dropWhile (`S.member` boxes) moves
    in  if S.member (head res) walls then state else State (head moves) walls (moveBox (head moves) (head res) boxes)

moveBox :: Coord -> Coord -> S.Set Coord -> S.Set Coord
moveBox from to = S.delete from . S.insert to

gpsSum :: State -> Int
gpsSum (State _ _ boxes) = sum $ S.map gpsCoord boxes

gpsCoord :: Coord -> Int
gpsCoord (x,y) = 100*y + x

step :: Direction -> Coord -> Coord
step DirRight (x,y) = (x+1,y)
step DirLeft (x,y) = (x-1,y)
step DirDown (x,y) = (x,y+1)
step DirUp (x,y) = (x,y-1)

solve2 :: (State, [Direction]) -> Int
solve2 (state, moves) = gpsSum $ foldl move2 state' moves
    where state' = expandMap state

move2 :: State -> Direction -> State
move2 state@(State loc walls boxes) dir = maybe state (State loc' walls) (move2' walls dir boxes loc')
        where loc' = step dir loc

pushBox :: S.Set Coord -> Direction -> S.Set Coord -> Coord -> Maybe (S.Set Coord)
pushBox walls dir boxes loc = do
        boxes' <- move2' walls dir (S.delete loc boxes) locLeft -- delete current box from set first, we need to prevent it colliding with itself
        boxes'' <- move2' walls dir boxes' locRight
        return (S.insert locLeft boxes'')
    where locLeft = step dir loc
          locRight = step DirRight locLeft

move2' :: S.Set Coord -> Direction -> S.Set Coord -> Coord -> Maybe (S.Set Coord)
move2' walls dir boxes loc
    | S.member loc walls = Nothing
    | otherwise = case boxesHit of
                    [] -> Just boxes
                    (boxLoc:_) -> pushBox walls dir boxes boxLoc
        where boxesHit = filter (`S.member` boxes) [loc, step DirLeft loc]
