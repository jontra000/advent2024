module Dijkstra16 (dijkstra) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (delete)

type Node a = M.Map a Int
type VisitedNodes a = S.Set a
type UnvisitedNodes a = M.Map a (Int, S.Set a)
type Nodes a = M.Map a (Node a)

dijkstra :: Ord a => Nodes a -> a -> [a] -> Maybe (Int, S.Set a)
dijkstra nodes start = dijkstra' nodes S.empty M.empty (Just (start, (0, S.singleton start))) Nothing

dijkstra' :: Ord a => Nodes a -> VisitedNodes a -> UnvisitedNodes a -> Maybe (a, (Int, S.Set a)) -> Maybe (Int, S.Set a) -> [a] -> Maybe (Int, S.Set a)
dijkstra' _ _ _ Nothing _ _ = Nothing
dijkstra' nodes visited unvisitedNodes (Just (currentNode, nodeState@(newDistance, newPaths))) prevResult targetNodes
    | currentNode `elem` targetNodes =
        let targetNodes' = currentNode `delete` targetNodes
            result = case prevResult of
                        Nothing -> Just nodeState
                        Just (prevDistance, prevPaths) ->
                            if prevDistance < newDistance
                            then prevResult
                            else if prevDistance == newDistance
                            then Just (prevDistance, S.union prevPaths newPaths)
                            else error "Should never encounter a shorter path than previously found"
        in  if null targetNodes' || maybe False ((< newDistance) . fst) prevResult
            then result
            else dijkstraStep nodes result targetNodes' visited unvisitedNodes'
    | otherwise = dijkstraStep nodes prevResult targetNodes visited unvisitedNodes'
        where unvisitedNodes' = updateUnvisited nodes nodeState currentNode visited unvisitedNodes

dijkstraStep :: Ord a => Nodes a -> Maybe (Int, S.Set a) -> [a] -> VisitedNodes a -> UnvisitedNodes a -> Maybe (Int, S.Set a)
dijkstraStep nodes prevResult targetNodes visited unvisitedNodes =
    let nextNode = smallestDistance unvisitedNodes
    in  dijkstra' nodes (updateVisited visited nextNode) unvisitedNodes nextNode prevResult targetNodes

updateVisited :: Ord a => S.Set a -> Maybe (a, b) -> S.Set a
updateVisited visited Nothing = visited
updateVisited visited (Just (n, _)) = S.insert n visited

updateUnvisited :: Ord a => Nodes a -> (Int, S.Set a) -> a ->  VisitedNodes a -> UnvisitedNodes a -> UnvisitedNodes a
updateUnvisited nodes currentDistance currentNode visited unvisitedNodes = M.delete currentNode $ updateNeighbours nodes currentDistance visited unvisitedNodes currentNode

updateNeighbours :: Ord a => Nodes a -> (Int, S.Set a) -> VisitedNodes a -> UnvisitedNodes a -> a -> UnvisitedNodes a
updateNeighbours nodes currentDistance visitedNodes unvisitedNodes = foldl (updateNeighbour currentDistance) unvisitedNodes . filter ((`S.notMember` visitedNodes) . fst) . M.toList . (nodes M.!)

updateNeighbour :: Ord a => (Int, S.Set a) -> UnvisitedNodes a -> (a, Int) -> UnvisitedNodes a
updateNeighbour (currentDistance, currentPath) unvisitedNodes (key, distance) =
    M.alter (updateDistance (currentDistance + distance) (S.insert key currentPath)) key unvisitedNodes

smallestDistance :: UnvisitedNodes a -> Maybe (a, (Int, S.Set a))
smallestDistance = M.foldlWithKey minTentativeDistance Nothing

updateDistance :: Ord a => Int -> S.Set a -> Maybe (Int, S.Set a) -> Maybe (Int, S.Set a)
updateDistance d path Nothing = Just (d, path)
updateDistance dNew pathNew old@(Just (dOld, pathOld))
    | dNew == dOld = Just (dOld, S.union pathNew pathOld)
    | dNew < dOld = Just (dNew, pathNew)
    | otherwise = old

minTentativeDistance :: Maybe (a, (Int, S.Set a)) -> a -> (Int, S.Set a) -> Maybe (a, (Int, S.Set a))
minTentativeDistance Nothing key prev = Just (key, prev)
minTentativeDistance prev@(Just (_, (prevDistance, _))) key next@(tentativeDistance, _)
    | tentativeDistance < prevDistance = Just (key, next)
    | otherwise = prev