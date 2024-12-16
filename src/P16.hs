module P16 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Lib (Direction(..), textToCoordMap, Coord)
import Dijkstra (dijkstra, Node(..))
import qualified Dijkstra16 as D16
import Data.Maybe (mapMaybe)

run1 = solve1 . parse

run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input16"

parse = textToCoordMap

solve1 :: M.Map Coord Char -> Maybe Int
solve1 m = dijkstra nodes' (start, DirRight) (map (\d -> (end, d)) [DirLeft, DirRight, DirUp, DirDown])
    where nodes' = nodes m
          start = head $ M.keys $ M.filter (=='S') m
          end = head $ M.keys $ M.filter (=='E') m

nodes :: M.Map Coord Char -> M.Map (Coord, Direction) (Node (Coord, Direction))
nodes m = M.fromList $ concatMap (node spaces) spaces
    where spaces = M.keysSet (M.filter (/='#') m)

node :: S.Set Coord -> Coord -> [((Coord, Direction), Node (Coord, Direction))]
node spaces c = map (dirNode spaces c) [DirLeft, DirRight, DirDown, DirUp]

dirNode :: S.Set Coord -> Coord -> Direction -> ((Coord, Direction), Node (Coord, Direction))
dirNode spaces c dir =
    let distanceMap = M.fromList $ mapMaybe (dirNode' spaces c dir) [DirLeft, DirRight, DirDown, DirUp]
    in  ((c, dir), Node 0 distanceMap)

dirNode' spaces c dirStart dirEnd
    | S.notMember c' spaces = Nothing
    | dirStart == dirEnd = Just ((c', dirEnd), 1)
    | isOpposite dirStart dirEnd = Just ((c', dirEnd), 2001)
    | otherwise = Just ((c', dirEnd), 1001)
        where c' = step c dirEnd

step (x,y) DirLeft = (x-1,y)
step (x,y) DirRight = (x+1,y)
step (x,y) DirDown = (x,y+1)
step (x,y) DirUp = (x,y-1)

isOpposite DirUp DirDown = True
isOpposite DirDown DirUp = True
isOpposite DirLeft DirRight = True
isOpposite DirRight DirLeft = True
isOpposite _ _ = False

solve2 m = combineBestPaths $ mapMaybe (D16.dijkstra nodes' (start, DirRight) . (\d -> (end, d))) [DirLeft, DirRight, DirUp, DirDown]
    where nodes' = mapNodes $ nodes m
          start = head $ M.keys $ M.filter (=='S') m
          end = head $ M.keys $ M.filter (=='E') m

mapNodes = M.map (\(Node a b) -> D16.Node a b)

combineBestPaths [] = Nothing
combineBestPaths results =
    let shortestPath = minimum (map fst results)
    in  Just $ length $ S.unions $ map (uniqueCoords . snd) $ filter ((==shortestPath) . fst) results

uniqueCoords = S.map fst