module P5 (run1, run2, inputLocation) where
import Data.List.Split (splitOn)
import Data.List (delete, find)
import Data.Maybe (fromJust)

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
solve2 input@(Input rules _) = sum $ map (middlePage . correctErrors rules) $ invalidBlocks input

invalidBlocks :: Input -> [Update]
invalidBlocks (Input rules updates) = filter (not . isValid rules) updates

correctErrors :: [Rule] -> Update -> [Int]
correctErrors _ [] = []
correctErrors rules block =
    let rules' = filter (ruleApplies block) rules
        next = firstItem rules' block
    in  next : correctErrors rules' (delete next block)

firstItem :: [Rule] -> Update -> Int
firstItem rules = fromJust . find (noPageBefore rules)

noPageBefore :: [Rule] -> Int -> Bool
noPageBefore rules i = not (any (hasPredecessor i) rules)

hasPredecessor :: Int -> Rule -> Bool
hasPredecessor i (_, after) = after == i

ruleApplies :: Update -> Rule -> Bool
ruleApplies update (before, after) = after `elem` update && before `elem` update