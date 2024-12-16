module Dijkstra16 (dijkstra, Node(..)) where

import qualified Data.Map as M
import qualified Data.Set as S

type DistanceMap a = M.Map a Int
data Node a = Node Int (DistanceMap a)
type UnvisitedNodes a = M.Map a (Maybe (Int, S.Set a))
type Nodes a = M.Map a (Node a)

dijkstra :: Ord a => Nodes a -> a -> a -> Maybe (Int, S.Set a)
dijkstra nodes start = dijkstra' nodes (M.map (const Nothing) nodes) (Just (start, (0, S.singleton start)))

dijkstra' :: Ord a => Nodes a -> UnvisitedNodes a -> Maybe (a, (Int, S.Set a)) -> a -> Maybe (Int, S.Set a)
dijkstra' _ _ Nothing _ = Nothing
dijkstra' nodes unvisitedNodes (Just (currentNode, nodeState)) targetNode
    | currentNode == targetNode = Just nodeState
    | otherwise = 
        let unvisitedNodes' = updateUnvisited nodes nodeState currentNode unvisitedNodes
        in  dijkstraStep nodes targetNode unvisitedNodes'

dijkstraStep :: Ord a => Nodes a -> a -> UnvisitedNodes a -> Maybe (Int, S.Set a)
dijkstraStep input targetNode unvisitedNodes =
    let nextNode = smallestDistance unvisitedNodes
    in  dijkstra' input unvisitedNodes nextNode targetNode

updateUnvisited :: Ord a => Nodes a -> (Int, S.Set a) -> a -> UnvisitedNodes a -> UnvisitedNodes a
updateUnvisited nodes currentDistance currentNode unvisitedNodes = M.delete currentNode $ updateNeighbours nodes currentDistance unvisitedNodes currentNode

updateNeighbours :: Ord a => Nodes a -> (Int, S.Set a) -> UnvisitedNodes a -> a -> UnvisitedNodes a
updateNeighbours nodes currentDistance unvisitedNodes = foldl (updateNeighbour currentDistance) unvisitedNodes . M.toList . reachableNeighbours nodes

reachableNeighbours :: Ord a => Nodes a -> a -> DistanceMap a
reachableNeighbours input nodeName = case input M.! nodeName of (Node _ neighbours) -> neighbours

updateNeighbour :: Ord a => (Int, S.Set a) -> UnvisitedNodes a -> (a, Int) -> UnvisitedNodes a
updateNeighbour (currentDistance, currentPath) unvisitedNodes (key, distance) =
    M.adjust (updateDistance (currentDistance + distance) (S.insert key currentPath)) key unvisitedNodes

smallestDistance :: UnvisitedNodes a -> Maybe (a, (Int, S.Set a))
smallestDistance = M.foldlWithKey minTentativeDistance Nothing

updateDistance :: Ord a => Int -> S.Set a -> Maybe (Int, S.Set a) -> Maybe (Int, S.Set a)
updateDistance d path Nothing = Just (d, path)
updateDistance dNew pathNew old@(Just (dOld, pathOld))
    | dNew == dOld = Just (dOld, S.union pathNew pathOld)
    | dNew < dOld = Just (dNew, pathNew)
    | otherwise = old

minTentativeDistance :: Maybe (a, (Int, S.Set a)) -> a -> Maybe (Int, S.Set a) -> Maybe (a, (Int, S.Set a))
minTentativeDistance prev _ Nothing = prev
minTentativeDistance Nothing key (Just prev) = Just (key, prev)
minTentativeDistance prev@(Just (_, (prevDistance, _))) key (Just next@(tentativeDistance, _))
    | tentativeDistance < prevDistance = Just (key, next)
    | otherwise = prev