module P18 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Lib (Coord)
import Dijkstra (dijkstra)
import Data.Maybe (isNothing)
import Data.List (inits, find)

run1 = solve1 . parse

run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input18"

parse = map parseLine . lines

parseLine = parseCoord . splitOn ","

parseCoord :: [String] -> Coord
parseCoord (x:y:_) = (read x, read y)

solve1 input = dijkstra (nodes (S.fromList $ take 1024 input)) (0,0) [(70,70)]

nodes walls = M.fromList $ map (\c -> (c, graph c (S.fromList freeNodes))) freeNodes
    where freeNodes = filter (`S.notMember` walls) [(x,y) | x <- [0..70], y <- [0..70]]

graph :: Coord -> S.Set Coord -> M.Map Coord Int
graph (x,y) freeNodes = M.fromList $ map (\c -> (c,1)) $ filter (`S.member` freeNodes) neighbours
    where neighbours = [(x+1,y),(x-1,y),(x,y-1),(x,y+1)]

solve input = dijkstra (nodes (S.fromList input)) (0,0) [(70,70)]

solve2 input = find (isNothing . solve) (drop 1024 $ inits input)