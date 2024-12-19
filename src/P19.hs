module P19 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (mapMaybe)
import Data.List.Split (splitOn)
import Data.List (isPrefixOf, find)
import Lib (memoize)

run1 = solve1 . parse

run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input19"

parse = parseTowels . lines

parseTowels (available:_:targets) = (splitOn ", " available, targets)

solve1 (available, targets) = length $ filter (canMake available) targets

canMake _ [] = True
canMake available target =
    let matches = filter (`isPrefixOf` target) available
    in  any (canMake available . (`drop` target) . length) matches

solve2 (available, targets) = fst $ solveMemo available M.empty targets

solveMemo :: [String] -> M.Map String Int -> [String] -> (Int, M.Map String Int)
solveMemo _ cache [] = (0, cache)
solveMemo available cache (next:targets) =
    let (result, cache') = solveSingle available cache next
        (result', cache'') = solveMemo available cache' targets
    in  (result + result', cache'')

solveSingle _ cache [] = (1, cache)
solveSingle available cache target =
    case M.lookup target cache of
        Just x -> (x, cache)
        Nothing ->
            let matches = filter (`isPrefixOf` target) available
                (result, cacheFinal) = foldl go (0, cache) matches
            in  (result, M.insert target result cacheFinal)
            where go (acc, cache') match =
                    let (result, cache'') = solveSingle available cache' (drop (length match) target)
                    in  (acc + result, cache'')