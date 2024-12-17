module Dijkstra (dijkstra) where

import qualified Data.Map as M
import qualified Data.Set as S

type Node a = M.Map a Int
type VisitedNodes a = S.Set a
type UnvisitedNodes a = M.Map a Int
type Nodes a = M.Map a (Node a)

dijkstra :: Ord a => Nodes a -> a -> [a] -> Maybe Int
dijkstra nodes start = dijkstra' nodes S.empty M.empty (Just (start, 0))

dijkstra' :: Ord a => Nodes a -> VisitedNodes a -> UnvisitedNodes a -> Maybe (a, Int) -> [a] -> Maybe Int
dijkstra' _ _ _ Nothing _ = Nothing
dijkstra' nodes visited unvisitedNodes (Just (currentNode, nodeState)) targetNodes
    | currentNode `elem` targetNodes = Just nodeState
    | otherwise = dijkstraStep nodes targetNodes visited unvisitedNodes'
        where unvisitedNodes' = updateUnvisited nodes nodeState currentNode visited unvisitedNodes

dijkstraStep :: Ord a => Nodes a -> [a] -> VisitedNodes a -> UnvisitedNodes a -> Maybe Int
dijkstraStep nodes targetNodes visited unvisitedNodes =
    let nextNode = smallestDistance unvisitedNodes
    in  dijkstra' nodes (updateVisited visited nextNode) unvisitedNodes nextNode targetNodes

updateVisited :: Ord a => S.Set a -> Maybe (a, b) -> S.Set a
updateVisited visited Nothing = visited
updateVisited visited (Just (n, _)) = S.insert n visited

updateUnvisited :: Ord a => Nodes a -> Int -> a ->  VisitedNodes a -> UnvisitedNodes a -> UnvisitedNodes a
updateUnvisited nodes currentDistance currentNode visited unvisitedNodes = M.delete currentNode $ updateNeighbours nodes currentDistance visited unvisitedNodes currentNode

updateNeighbours :: Ord a => Nodes a -> Int -> VisitedNodes a -> UnvisitedNodes a -> a -> UnvisitedNodes a
updateNeighbours nodes currentDistance visitedNodes unvisitedNodes = foldl (updateNeighbour currentDistance) unvisitedNodes . filter ((`S.notMember` visitedNodes) . fst) . M.toList . (nodes M.!)

updateNeighbour :: Ord a => Int -> UnvisitedNodes a -> (a, Int) -> UnvisitedNodes a
updateNeighbour currentDistance unvisitedNodes (key, distance) =
    M.alter (updateDistance (currentDistance + distance)) key unvisitedNodes

smallestDistance :: UnvisitedNodes a -> Maybe (a, Int)
smallestDistance = M.foldlWithKey minTentativeDistance Nothing

updateDistance :: Int -> Maybe Int -> Maybe Int
updateDistance d Nothing = Just d
updateDistance dNew (Just dOld) = Just (min dNew dOld)

minTentativeDistance :: Maybe (a, Int) -> a -> Int -> Maybe (a, Int)
minTentativeDistance Nothing key tentativeDistance = Just (key, tentativeDistance)
minTentativeDistance prev@(Just (_, prevDistance)) key tentativeDistance
    | tentativeDistance < prevDistance = Just (key, tentativeDistance)
    | otherwise = prev