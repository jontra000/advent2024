module P23BK (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (nub, sort, intercalate, isPrefixOf)
import Data.Maybe (mapMaybe)

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
setsOf3 [] = []
setsOf3 ((a,b):vs) =
            let neighboursA = connects a vs
                neighboursB = connects b vs
            in  map (\n -> [a,b,n]) (filter (`elem` neighboursB) neighboursA) ++ setsOf3 vs

connects :: String -> [(String, String)] -> [String]
connects n = mapMaybe (\(a,b) -> if a == n then Just b else if b == n then Just a else Nothing)

containsT :: [String] -> Bool
containsT = any ("t" `isPrefixOf`)

solve2 :: [(String, String)] -> String
solve2 = password . largestClique

largestClique :: [(String, String)] -> S.Set String
largestClique xs =
    let names = sort $ nub $ concatMap (\(a,b) -> [a,b]) xs
        network = M.fromList $ map (\n -> (n, S.fromList (connections xs n))) names
    in  bronKerbosch network S.empty (S.fromList names) S.empty

connections :: [(String, String)] -> String -> [String]
connections xs name = map snd (filter ((==name) . fst) xs) ++ map fst (filter ((==name) . snd) xs)

password :: S.Set String -> String
password = intercalate "," . sort . S.toList

bronKerbosch :: M.Map String (S.Set String) -> S.Set String -> S.Set String -> S.Set String -> S.Set String
bronKerbosch network r p x
    | null p && null x = r
    | otherwise =
        let (res, _, _, _) = foldl go (S.empty, r, p, x) p
        in  res
        where go (bestClique, r', p', x') v =
                let clique = bronKerbosch network (S.insert v r') (p' `S.intersection` neighbors v) (x' `S.intersection` neighbors v)
                    nextClique = if S.size clique > S.size bestClique then clique else bestClique
                in  (nextClique, r', S.delete v p', S.insert v x')
              neighbors v = network M.! v