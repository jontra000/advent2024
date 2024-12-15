module P15 (run1, run2, inputLocation) where
import Data.List.Split (splitOn)
import Lib (Coord, textToCoordMap)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.List (find)

type Map = M.Map Lib.Coord Char
data State = State Coord Map deriving Show

run1 = solve1 . parse

run2 = solve2 . parse2

inputLocation :: String
inputLocation = "inputs/input15"

parse = parseBlock . splitOn [""] . lines

parse2 = parseBlock2 . splitOn [""] . lines

parseBlock :: [[String]] -> (State, String)
parseBlock (mapInput:moveInput:_) = (parseMap mapInput, parseMoves moveInput)

parseBlock2 (mapInput:moveInput:_) = (parseMap2 mapInput, parseMoves moveInput)

parseMap input = State robotLoc m
    where m = textToCoordMap $ unlines input
          robotLoc = fst $ fromJust $ find ((=='@') . snd) $ M.toList m

parseMap2 = parseMap . expandMap

expandMap = map go
    where go = concatMap expandItem

expandItem 'O' = "[]"
expandItem '@' = "@."
expandItem x = [x,x]

parseMoves = concat

solve1 (state, moves) = gpsSum $ foldl move state moves

move state@(State loc m) dir =
    let moves = tail $ iterate (step dir) loc
        res = dropWhile ((=='O') . (m M.!)) moves
    in  case (map (m M.!) res) of
            ('#':_) -> state
            _ -> State (head moves) (M.insert (head moves) '.' (M.insert (head res) 'O' m))

gpsSum (State _ m) = sum $ map gpsCoord $ M.toList m

gpsCoord ((x,y), 'O') = 100*y + x
gpsCoord ((x,y), '[') = 100*y + x
gpsCoord _ = 0

step :: Char -> Coord -> Coord
step '>' (x,y) = (x+1,y)
step '<' (x,y) = (x-1,y)
step 'v' (x,y) = (x,y+1)
step '^' (x,y) = (x,y-1)


solve2 (state, moves) = gpsSum $ foldl move2 state moves

move2 state@(State loc m) dir = case move2' m dir loc' '.' of
                            Nothing -> state
                            Just m' -> State loc' m'
        where loc' = step dir loc


move2' m dir loc item = case m M.! loc of
                        '#' -> Nothing
                        '[' -> if dir == '<' || dir == '>'
                                then case move2' m dir loc' '[' of
                                        Nothing -> Nothing
                                        Just m' -> Just  (M.insert loc item m')
                                else do
                                    m' <- move2' m dir loc' '['
                                    m'' <- move2' m' dir (step '>' loc') ']'
                                    return (M.insert loc item (M.insert (step '>' loc) '.' m''))
                        ']' -> if dir == '<' || dir == '>'
                                then case move2' m dir loc' ']' of
                                        Nothing -> Nothing
                                        Just m' -> Just  (M.insert loc item m')
                                else do
                                    m' <- move2' m dir loc' ']'
                                    m'' <- move2' m' dir (step '<' loc') '['
                                    return (M.insert loc item (M.insert (step '<' loc) '.' m''))
                        _ -> Just (M.insert loc item m)
    where loc' = step dir loc