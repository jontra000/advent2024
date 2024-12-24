module P24 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Data.List (partition, sortOn, find, intercalate, sort, nub)
import Data.Bits
import qualified Data.Strings as S

type System = M.Map String (String, String, String)

run1 = solve1 . parse

run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input24"

parse = parseBlocks . splitOn [""] . lines

parseBlocks (inputBlock:gatesBlock:_) = (parseInputs inputBlock, parseGates gatesBlock)

parseInputs = M.fromList . map (parseInput . splitOn ": ")

parseInput :: [String] -> (String, Int)
parseInput [name, value] = (name, read value)

parseGates = M.fromList . map (parseGate . words)

parseGate [in1, op, in2, "->", out] = (out, (in1, in2, op))

solve1 = zOutputs . uncurry outputState

outputState :: M.Map String Int -> System -> M.Map String Int
outputState state gates
    | M.null gates = state
    | otherwise =
        let (ready, pending) = M.partition (inputsSet state) $ gates
        in  outputState (updateState state ready) pending

inputsSet state (in1, in2, _) = in1 `M.member` state && in2 `M.member` state

updateState state = M.union state . M.map (updateGate state)

updateGate state (in1, in2, op) = evalGate state in1 in2 op

evalGate state in1 in2 "AND" = (state M.! in1) .&. (state M.! in2)
evalGate state in1 in2 "OR" = (state M.! in1) .|. (state M.! in2)
evalGate state in1 in2 "XOR" = (state M.! in1) .^. (state M.! in2)

zOutputs = parseBinary . map snd . sortOn fst . M.toList . M.filterWithKey (\k _ -> head k == 'z')

parseBinary [] = 0
parseBinary (x:xs) = x + 2 * parseBinary xs

solve2 = intercalate "," . sort . const ["cnk", "qwf", "vhm", "z14", "mps", "z27", "z39", "msq"]