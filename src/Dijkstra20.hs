module Dijkstra20 (dijkstra) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (delete)

type Node a = M.Map a Int
type VisitedNodes a = S.Set a
type UnvisitedNodes a = M.Map a (Int, [a])
type Nodes a = M.Map a (Node a)

dijkstra :: Ord a => Nodes a -> a -> [a] -> Maybe (Int, [a])
dijkstra nodes start = dijkstra' nodes S.empty M.empty (Just (start, (0, [start]))) Nothing

dijkstra' :: Ord a => Nodes a -> VisitedNodes a -> UnvisitedNodes a -> Maybe (a, (Int, [a])) -> Maybe (Int, [a]) -> [a] -> Maybe (Int, [a])
dijkstra' _ _ _ Nothing _ _ = Nothing
dijkstra' nodes visited unvisitedNodes (Just (currentNode, nodeState@(newDistance, newPaths))) prevResult targetNodes
    | currentNode `elem` targetNodes = Just (newDistance, reverse newPaths)
    | otherwise = dijkstraStep nodes prevResult targetNodes visited unvisitedNodes'
        where unvisitedNodes' = updateUnvisited nodes nodeState currentNode visited unvisitedNodes

dijkstraStep :: Ord a => Nodes a -> Maybe (Int, [a]) -> [a] -> VisitedNodes a -> UnvisitedNodes a -> Maybe (Int, [a])
dijkstraStep nodes prevResult targetNodes visited unvisitedNodes =
    let nextNode = smallestDistance unvisitedNodes
    in  dijkstra' nodes (updateVisited visited nextNode) unvisitedNodes nextNode prevResult targetNodes

updateVisited :: Ord a => S.Set a -> Maybe (a, b) -> S.Set a
updateVisited visited Nothing = visited
updateVisited visited (Just (n, _)) = S.insert n visited

updateUnvisited :: Ord a => Nodes a -> (Int, [a]) -> a ->  VisitedNodes a -> UnvisitedNodes a -> UnvisitedNodes a
updateUnvisited nodes currentDistance currentNode visited unvisitedNodes = M.delete currentNode $ updateNeighbours nodes currentDistance visited unvisitedNodes currentNode

updateNeighbours :: Ord a => Nodes a -> (Int, [a]) -> VisitedNodes a -> UnvisitedNodes a -> a -> UnvisitedNodes a
updateNeighbours nodes currentDistance visitedNodes unvisitedNodes = foldl (updateNeighbour currentDistance) unvisitedNodes . filter ((`S.notMember` visitedNodes) . fst) . M.toList . (nodes M.!)

updateNeighbour :: Ord a => (Int, [a]) -> UnvisitedNodes a -> (a, Int) -> UnvisitedNodes a
updateNeighbour (currentDistance, currentPath) unvisitedNodes (key, distance) =
    M.alter (updateDistance (currentDistance + distance) (key  : currentPath)) key unvisitedNodes

smallestDistance :: UnvisitedNodes a -> Maybe (a, (Int, [a]))
smallestDistance = M.foldlWithKey minTentativeDistance Nothing

updateDistance :: Ord a => Int -> [a] -> Maybe (Int, [a]) -> Maybe (Int,[a])
updateDistance d path Nothing = Just (d, path)
updateDistance dNew pathNew old@(Just (dOld, _))
    | dNew < dOld = Just (dNew, pathNew)
    | otherwise = old

minTentativeDistance :: Maybe (a, (Int, [a])) -> a -> (Int, [a]) -> Maybe (a, (Int, [a]))
minTentativeDistance Nothing key prev = Just (key, prev)
minTentativeDistance prev@(Just (_, (prevDistance, _))) key next@(tentativeDistance, _)
    | tentativeDistance < prevDistance = Just (key, next)
    | otherwise = prev