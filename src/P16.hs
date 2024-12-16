module P16 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Lib (Direction(..), textToCoordMap, Coord)
import Dijkstra (dijkstra)
import qualified Dijkstra16 as D16
import Data.Maybe (mapMaybe)

type NodeKey = (Coord, Direction)

run1 :: String -> Maybe Int
run1 = solve1 . parse

run2 :: String -> Maybe Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input16"

parse :: String -> M.Map Coord Char
parse = textToCoordMap

solve1 :: M.Map Coord Char -> Maybe Int
solve1 = solve dijkstra

solve :: (M.Map NodeKey (M.Map NodeKey Int)  -> (Coord, Direction) -> [NodeKey] -> t) -> M.Map Coord Char -> t
solve dijkstraImpl m = dijkstraImpl nodes' start endNodes
    where start = (head $ M.keys $ M.filter (=='S') m, DirRight)
          end = head $ M.keys $ M.filter (=='E') m
          nodes' = nodes m start
          endNodes = filter (`M.member` nodes') (map (\d -> (end, d)) [DirLeft, DirRight, DirUp, DirDown])

nodes :: M.Map Coord Char -> NodeKey -> M.Map NodeKey (M.Map NodeKey Int)
nodes m start = removeUnreachableNodes start $ M.fromList $ concatMap (node spaces) spaces
    where spaces = M.keysSet (M.filter (/='#') m)

removeUnreachableNodes :: NodeKey -> M.Map NodeKey (M.Map NodeKey Int) -> M.Map NodeKey (M.Map NodeKey Int)
removeUnreachableNodes start m = M.intersection m (M.insert start 0 $ M.unions $ M.elems m)

node :: S.Set Coord -> Coord -> [(NodeKey, M.Map NodeKey Int)]
node spaces c = map (dirNode spaces c) [DirLeft, DirRight, DirDown, DirUp]

dirNode :: S.Set Coord -> Coord -> Direction -> (NodeKey, M.Map NodeKey Int)
dirNode spaces c dir =
    let distanceMap = M.fromList $ mapMaybe (dirNode' spaces c dir) [DirLeft, DirRight, DirDown, DirUp]
    in  ((c, dir), distanceMap)

dirNode' :: S.Set Coord -> Coord -> Direction -> Direction -> Maybe (NodeKey, Int)
dirNode' spaces c dirStart dirEnd
    | S.notMember c' spaces = Nothing
    | dirStart == dirEnd = Just ((c', dirEnd), 1)
    | isOpposite dirStart dirEnd = Just ((c', dirEnd), 2001)
    | otherwise = Just ((c', dirEnd), 1001)
        where c' = step c dirEnd

step :: Coord -> Direction -> Coord
step (x,y) DirLeft = (x-1,y)
step (x,y) DirRight = (x+1,y)
step (x,y) DirDown = (x,y+1)
step (x,y) DirUp = (x,y-1)

isOpposite :: Direction -> Direction -> Bool
isOpposite DirUp DirDown = True
isOpposite DirDown DirUp = True
isOpposite DirLeft DirRight = True
isOpposite DirRight DirLeft = True
isOpposite _ _ = False

solve2 :: M.Map Coord Char -> Maybe Int
solve2 = fmap uniqueNodes . solve D16.dijkstra

uniqueNodes :: (Int, S.Set NodeKey) -> Int
uniqueNodes (_, path) = length (S.map fst path)