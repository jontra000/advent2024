module P5CustomSort (run1, run2, inputLocation) where
import Data.List.Split (splitOn)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)

type Rule = (Int, Int)
type Update = [Int]
data Input = Input [Rule] [Update]

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input5"

parse :: String -> Input
parse = parseBlocks . splitOn [""] . lines

parseBlocks :: [[String]] -> Input
parseBlocks (ruleBlock:update:_) = Input (map (parseRule . splitOn "|") ruleBlock) (map parseUpdate update)
parseBlocks e = error ("Unexpected input " ++ show e)

parseRule :: [String] -> Rule
parseRule (a:b:_) = (read a, read b)
parseRule e = error ("Badly formatted rule" ++ show e)

parseUpdate :: String -> Update
parseUpdate = map read . splitOn ","

solve1 :: Input -> Int
solve1 = sum . map middlePage . validBlocks

validBlocks :: Input -> [Update]
validBlocks (Input rules updates) = filter (isValid rules) updates

isValid :: [Rule] -> Update -> Bool
isValid rules update = all (rulePasses update) rules

rulePasses :: Update -> Rule -> Bool
rulePasses update rule@(before, after) = not (ruleApplies update rule) || before `elem` takeWhile (/=after) update

middlePage :: Update -> Int
middlePage xs = xs !! (length xs `div` 2)

solve2 :: Input -> Int
solve2 (Input rules updates) = sum $ mapMaybe (middlePageIfError (customSorter rules)) updates

middlePageIfError :: (Int -> Int -> Ordering) -> [Int] -> Maybe Int
middlePageIfError sorter update =
    let sortedUpdate = sortBy sorter update
    in  if sortedUpdate == update
        then Nothing
        else Just (middlePage sortedUpdate) 

customSorter :: [Rule] -> Int -> Int -> Ordering
customSorter [] _ _ = EQ
customSorter ((before,after):rules) a b
    | before == a && after == b = LT
    | after == a && before == b = GT
    | otherwise = customSorter rules a b

ruleApplies :: Update -> Rule -> Bool
ruleApplies update (before, after) = after `elem` update && before `elem` update