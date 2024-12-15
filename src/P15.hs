module P15 (run1, run2, inputLocation) where
import Data.List.Split (splitOn)
import Lib (Coord, textToCoordMap)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.List (find)

type Map = M.Map Lib.Coord Char
data State = State Coord Map deriving Show

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse2

inputLocation :: String
inputLocation = "inputs/input15"

parse :: String -> (State, String)
parse = parseBlock . splitOn [""] . lines

parse2 :: String -> (State, String)
parse2 = parseBlock2 . splitOn [""] . lines

parseBlock :: [[String]] -> (State, String)
parseBlock (mapInput:moveInput:_) = (parseMap mapInput, parseMoves moveInput)
parseBlock _ = error "Bad input"

parseBlock2 :: [[String]] -> (State, [Char])
parseBlock2 (mapInput:moveInput:_) = (parseMap2 mapInput, parseMoves moveInput)
parseBlock2 _ = error "Bad input"

parseMap :: [String] -> State
parseMap input = State robotLoc m
    where m = textToCoordMap $ unlines input
          robotLoc = fst $ fromJust $ find ((=='@') . snd) $ M.toList m

parseMap2 :: [String] -> State
parseMap2 = parseMap . expandMap

expandMap :: [String] -> [String]
expandMap = map go
    where go = concatMap expandItem

expandItem :: Char -> String
expandItem 'O' = "[]"
expandItem '@' = "@."
expandItem x = [x,x]

parseMoves :: [[a]] -> [a]
parseMoves = concat

solve1 :: (State, [Char]) -> Int
solve1 (state, moves) = gpsSum $ foldl move state moves

move :: State -> Char -> State
move state@(State loc m) dir =
    let moves = tail $ iterate (step dir) loc
        res = dropWhile ((=='O') . (m M.!)) moves
    in  case map (m M.!) res of
            ('#':_) -> state
            _ -> State (head moves) (M.insert (head moves) '.' (M.insert (head res) 'O' m))

gpsSum :: State -> Int
gpsSum (State _ m) = sum $ map gpsCoord $ M.toList m

gpsCoord :: Num a => ((a, a), Char) -> a
gpsCoord ((x,y), 'O') = 100*y + x
gpsCoord ((x,y), '[') = 100*y + x
gpsCoord _ = 0

step :: Char -> Coord -> Coord
step '>' (x,y) = (x+1,y)
step '<' (x,y) = (x-1,y)
step 'v' (x,y) = (x,y+1)
step '^' (x,y) = (x,y-1)
step c _ = error ("Invalid step char: " ++ [c])

solve2 :: (State, [Char]) -> Int
solve2 (state, moves) = gpsSum $ foldl move2 state moves

move2 :: State -> Char -> State
move2 state@(State loc m) dir = case move2' m dir loc' '.' of
                            Nothing -> state
                            Just m' -> State loc' m'
        where loc' = step dir loc

move2' :: Map -> Char -> Coord -> Char -> Maybe Map
move2' m dir loc item 
    | c == '#' = Nothing
    | (c == '[' || c == ']') && (dir == '<' || dir == '>') = do
        m' <- move2' m dir loc' c
        return (M.insert loc item m')
    | c == '[' = do
        m' <- move2' m dir loc' c
        m'' <- move2' m' dir (step '>' loc') ']'
        return (M.insert loc item (M.insert (step '>' loc) '.' m''))
    | c == ']' = do
        m' <- move2' m dir loc' c
        m'' <- move2' m' dir (step '<' loc') '['
        return (M.insert loc item (M.insert (step '<' loc) '.' m''))
    | otherwise = Just (M.insert loc item m)
    where loc' = step dir loc
          c = m M.! loc