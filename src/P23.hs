module P23 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (nub, sort, partition, intercalate, maximumBy, group, tails, intersect)
import Data.Function (on)

run1 = solve1 . parse

run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input23"

parse = map parseLine . lines

parseLine (a:b:_:c:d:_) = ([a,b], [c,d])

solve1 = length . setsOf3

setsOf3 xs =
    let names = nub $ concatMap (\(a,b) -> [a,b]) xs
    in  pruneDups $ concatMap (setsOf3' xs) $ filter ((=='t' ) . head) names

setsOf3' xs name =
    let connections' = connections xs name
    in  map (\(a,b) -> (name, a ,b)) $ filter (\(a,b) -> a `elem` connections' && b `elem` connections') xs

connections xs name = map snd (filter ((==name) . fst) xs) ++ map fst (filter ((==name) . snd) xs)

pruneDups = nub . map (sort . (\(a,b,c) -> [a,b,c]))

-- solve2 = password . biggestGroup
solve2 = password . uncurry (:) . head . last . takeWhile (not . null) . combineLinksSeries

password :: [String] -> String
password = intercalate "," . sort

combineLinksSeries :: [(String, String)] -> [[(String, [String])]]
combineLinksSeries xs =
    let directLinks = S.fromList $ concatMap (\(x,y) -> [(x,y), (y,x)]) xs
        names = sort $ nub $ concatMap (\(a,b) -> [a,b]) xs
        initialLinks = map (\n -> (n, [])) names
    in  iterate (combineLinks directLinks) initialLinks

combineLinks _ [] = []
combineLinks directLinks ((x,links):xs) =
    let possibleMatches = map fst $ filter ((==links) . snd) xs
        matches = filter ((`S.member` directLinks) . (\y -> (x,y))) possibleMatches
    in  map (\match -> (x, match : links)) matches ++ combineLinks directLinks xs