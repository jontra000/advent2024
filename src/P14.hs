module P14 (run1, run2, inputLocation) where
import Data.List.Split (splitOn)
import Lib (Coord, addCoords, mulCoord)
import qualified Data.Set as S
import Data.List (transpose)

data Robot = Robot { location :: Coord, velocity :: Coord }

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input14"

gridWidth :: Int
gridWidth = 101

gridHeight :: Int
gridHeight = 103

parse :: String -> [Robot]
parse = map parseLine . lines

parseLine :: String -> Robot
parseLine = parseWords . words

parseWords :: [String] -> Robot
parseWords (posS:velS:_) = Robot (parseCoord posS) ( parseCoord velS)
parseWords s = error ("Bad input: " ++ unwords s)

parseCoord :: String -> Coord
parseCoord = parseCoord' . splitOn "," . drop 2

parseCoord' :: [String] -> Coord
parseCoord' (x:y:_) = (read x, read y)
parseCoord' e = error ("Bad coordinate: " ++ unwords e)

solve1 :: [Robot] -> Int
solve1 = safetyFactor . elapseTime 100

elapseTime :: Int -> [Robot] -> [Coord]
elapseTime t = map (wrapCoords . move t)

move :: Int -> Robot -> Coord
move t (Robot location velocity) = addCoords location (mulCoord t velocity)

wrapCoords :: Coord -> Coord
wrapCoords (x,y) = (x `mod` gridWidth, y `mod` gridHeight)

safetyFactor :: [Coord] -> Int
safetyFactor state = product $ map (`quadrantCount` state) quadrants

quadrantCount :: (Coord -> Bool) -> [Coord] -> Int
quadrantCount quadrantPredicate = length . filter quadrantPredicate

quadrants :: [Coord -> Bool]
quadrants = [\(x,y) -> x < xHalf && y < yHalf, \(x,y) -> x > xHalf && y < yHalf, \(x,y) -> x < xHalf && y > yHalf, \(x,y) -> x > xHalf && y > yHalf]
    where xHalf = gridWidth `div` 2
          yHalf = gridHeight `div` 2

solve2 :: [Robot] -> Int
solve2 = findTreeTime . map S.fromList . transpose . map robotWalk

findTreeTime :: [S.Set Coord] -> Int
findTreeTime = fst . head . filter (isTree . snd) . zip [0..]

isTree :: S.Set Coord -> Bool
isTree state = all (`S.member` state) treeCoords

treeCoords :: [Coord]
treeCoords = map (\x -> (x, 28)) [28..58]

robotWalk :: Robot -> [Coord]
robotWalk robot = iterate go (location robot)
    where go = wrapCoords . addCoords (velocity robot)