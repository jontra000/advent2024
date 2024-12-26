module P24 (run1, run2, inputLocation) where

import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.List (sortOn, find, intercalate, sort)
import Data.Bits
import qualified Data.Strings as S

type System = M.Map String (String, String, String)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> String
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input24"

parse :: String -> (M.Map String Int, System)
parse = parseBlocks . splitOn [""] . lines

parseBlocks :: [[String]] -> (M.Map String Int, System)
parseBlocks (inputBlock:gatesBlock:_) = (parseInputs inputBlock, parseGates gatesBlock)
parseBlocks _ = error "Invalid input format"

parseInputs :: [String] -> M.Map String Int
parseInputs = M.fromList . map (parseInput . splitOn ": ")

parseInput :: [String] -> (String, Int)
parseInput [name, value] = (name, read value)
parseInput _ = error "Invalid input format"

parseGates :: [String] -> System
parseGates = M.fromList . map (parseGate . words)

parseGate :: [String] -> (String, (String, String, String))
parseGate [in1, op, in2, "->", out] = (out, (in1, in2, op))
parseGate _ = error "Invalid gate format"

solve1 :: (M.Map String Int, System) -> Int
solve1 = zOutputs . uncurry outputState

outputState :: M.Map String Int -> System -> M.Map String Int
outputState state gates
    | M.null gates = state
    | otherwise =
        let (ready, pending) = M.partition (inputsSet state) $ gates
        in  outputState (updateState state ready) pending

inputsSet :: M.Map String Int -> (String, String, String) -> Bool
inputsSet state (in1, in2, _) = in1 `M.member` state && in2 `M.member` state

updateState :: M.Map String Int -> System -> M.Map String Int
updateState state = M.union state . M.map (updateGate state)

updateGate :: M.Map String Int -> (String, String, String) -> Int
updateGate state (in1, in2, op) = evalGate state in1 in2 op

evalGate :: M.Map String Int -> String -> String -> String -> Int
evalGate state in1 in2 "AND" = (state M.! in1) .&. (state M.! in2)
evalGate state in1 in2 "OR" = (state M.! in1) .|. (state M.! in2)
evalGate state in1 in2 "XOR" = (state M.! in1) .^. (state M.! in2)
evalGate _ _ _ op = error ("Invalid gate operation: " ++ op)

zOutputs :: M.Map String Int -> Int
zOutputs = parseBinary . map snd . sortOn fst . M.toList . M.filterWithKey (\k _ -> head k == 'z')

parseBinary :: [Int] -> Int
parseBinary [] = 0
parseBinary (x:xs) = x + 2 * parseBinary xs

solve2 :: (a, System) -> [Char]
solve2 = intercalate "," . sort . fixSystem 0 "" . snd

swapOutputs :: String -> String -> System -> System
swapOutputs a b system =
    let aVal = system M.! a
        bVal = system M.! b
    in  M.insert a bVal $ M.insert b aVal system

fixSystem :: Int -> String -> System -> [String]
fixSystem 0 _ system = fixSystem 1 inputCarry system -- Assume first gate is correct. If it was incorrect the output would be ambiguous.
    where z = "z00"
          inputCarry = findInputs (M.delete z system) "x00" "y00" "AND"

fixSystem n inputCarry system = 
    case tryFindInputs system (inputX n) (inputY n) "XOR" of
        Nothing -> []
        Just inputsXor ->
                if op == "XOR" && (sort [in1, in2] == sort [inputsXor, inputCarry])
                then fixCarry n system inputCarry inputsXor
                -- output is incorrect
                else if op /= "XOR" || (inputsXor `notElem` [in1, in2] && inputCarry `notElem` [in1, in2]) 
                then
                    let out = findInputs system inputsXor inputCarry "XOR"
                        system' = swapOutputs z out system
                    in  [out, z] ++ fixSystem n inputCarry system'
                -- one of the inputs is incorrect
                else 
                    let badInput1 = head $ filter (`notElem` [in1, in2]) [inputsXor, inputCarry]
                        badInput2 = head $ filter (`notElem` [inputsXor, inputCarry]) [in1, in2]
                        system' = swapOutputs badInput1 badInput2 system
                    in  [badInput1, badInput2] ++ fixSystem n inputCarry system'
            where z = "z" ++ S.strPadLeft '0' 2 (show n)
                  (in1, in2, op) = system M.! z

fixCarry :: Int -> System -> String -> String -> [String]
fixCarry n system inputCarry inputsXor = 
    let input1 = findInputs system inputCarry inputsXor "AND"
        input2 = findInputs system (inputX n) (inputY n) "AND"
    in  case tryFindInputs system input1 input2 "OR" of
            Nothing ->
                let (badInput, goodInput) = findWrongInput system input1 input2 "OR"
                    system' = swapOutputs badInput goodInput system
                in  [badInput, goodInput] ++ fixCarry n system' inputCarry inputsXor
            Just out ->
                fixSystem (n+1) out system

tryFindInputs :: System -> String -> String -> String -> Maybe String
tryFindInputs system in1 in2 op = fst <$> find (matchInputs in1 in2 op . snd) (M.toList system)

findWrongInput :: System -> String -> String -> String -> (String, String)
findWrongInput system in1 in2 op = 
    case find (matchInput in1 in2 op . snd) (M.toList system) of
        Nothing -> error ("No partial matching input: " ++ show (in1, in2, op))
        Just (_, (in1', in2', _)) ->
            if in1' == in1
            then (in2, in2')
            else if in1' == in2
            then (in1, in2')
            else if in2' == in1
            then (in2, in1')
            else (in1, in1')

matchInput :: String -> String -> String -> (String, String, String) -> Bool
matchInput in1 in2 op (in1', in2', op') = (in1 `elem` [in1', in2'] || in2 `elem` [in1', in2']) && op == op'

findInputs :: System -> String -> String -> String -> String
findInputs system in1 in2 op =
    case find (matchInputs in1 in2 op . snd) $ M.toList system of
        Nothing -> error ("No matching input: " ++ show (in1, in2, op))
        Just (out, _) -> out

matchInputs :: String -> String -> String -> (String, String, String) -> Bool
matchInputs in1 in2 op (in1', in2', op') = ((in1 == in1' && in2 == in2') || (in1 == in2' && in2 == in1')) && op == op'

inputX :: Int -> String
inputX = inputTerm 'x'

inputY :: Int -> String
inputY = inputTerm 'y'

inputTerm :: Char -> Int -> String
inputTerm p n = p : S.strPadLeft '0' 2 (show n)