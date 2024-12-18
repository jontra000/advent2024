module P18 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Lib (Coord)
import Dijkstra (dijkstra)
import Data.Maybe (isNothing)
import Data.List (inits)

run1 :: String -> Maybe Int
run1 = solve1 . parse

run2 :: String -> Maybe Coord
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input18"

parse :: String -> [Coord]
parse = map parseLine . lines

parseLine :: String -> Coord
parseLine = parseCoord . splitOn ","

parseCoord :: [String] -> Coord
parseCoord (x:y:_) = (read x, read y)
parseCoord e = error ("Bad coord input: " ++ show e)

solve1 :: [Coord] -> Maybe Int
solve1 = solve . take 1024

nodes :: S.Set Coord -> M.Map Coord (M.Map Coord Int)
nodes walls = M.fromList $ map (\c -> (c, graph (S.fromList freeNodes) c)) freeNodes
    where freeNodes = filter (`S.notMember` walls) [(x,y) | x <- [0..70], y <- [0..70]]

graph :: S.Set Coord -> Coord -> M.Map Coord Int
graph freeNodes = M.fromList . map (\c -> (c,1)) . filter (`S.member` freeNodes) . neighbours

neighbours :: Coord -> [Coord]
neighbours (x,y) = [(x+1,y),(x-1,y),(x,y-1),(x,y+1)]

solve :: [Coord] -> Maybe Int
solve input = dijkstra (nodes (S.fromList input)) (0,0) [(70,70)]

solve2 :: [Coord] -> Maybe Coord
solve2 = fmap last . binarySearch (isNothing . solve) . inits

binarySearch :: ([Coord] -> Bool) -> [[Coord]] -> Maybe [Coord]
binarySearch predicate xs = go 0 (length xs - 1)
  where
    go lo hi
      | lo == (hi - 1) = Just (xs !! hi)
      | lo > hi = Nothing
      | otherwise =
          let mid = (lo + hi) `div` 2
              midVal = xs !! mid
          in if predicate midVal then go lo mid else go mid hi 
