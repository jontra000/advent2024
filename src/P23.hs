module P23 (run1, run2, inputLocation) where

import qualified Data.Set as S
import Data.List (nub, sort, intercalate, isPrefixOf)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> String
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input23"

parse :: String -> [(String, String)]
parse = map parseLine . lines

parseLine :: String -> (String, String)
parseLine (a:b:_:c:d:_) = ([a,b], [c,d])
parseLine x = error $ "Invalid input: " ++ x

solve1 :: [(String, String)] -> Int
solve1 = length . filter containsT . setsOf3

setsOf3 :: [(String, String)] -> [[String]]
setsOf3 = map (uncurry (:)) . (!! 2) . combineLinksSeries

containsT :: [String] -> Bool
containsT = any ("t" `isPrefixOf`)

solve2 :: [(String, String)] -> String
solve2 = password . uncurry (:) . head . last . takeWhile (not . null) . combineLinksSeries

password :: [String] -> String
password = intercalate "," . sort

combineLinksSeries :: [(String, String)] -> [[(String, [String])]]
combineLinksSeries xs =
    let directLinks = S.fromList $ concatMap (\(x,y) -> [(x,y), (y,x)]) xs
        names = sort $ nub $ concatMap (\(a,b) -> [a,b]) xs
        initialLinks = map (\n -> (n, [])) names
    in  iterate (combineLinks directLinks) initialLinks

combineLinks :: S.Set (String, String) -> [(String, [String])] -> [(String, [String])]
combineLinks _ [] = []
combineLinks directLinks ((x,links):xs) =
    let possibleMatches = map fst $ filter ((==links) . snd) xs
        matches = filter ((`S.member` directLinks) . (\y -> (x,y))) possibleMatches
    in  map (\match -> (x, match : links)) matches ++ combineLinks directLinks xs