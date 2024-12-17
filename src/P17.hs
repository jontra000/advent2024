module P17 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bits
import Data.List.Split (splitOn)
import Data.List (intercalate, find, inits, tails)
import Data.Maybe (mapMaybe, fromJust)

data Registers = Registers Int Int Int deriving Show

run1 = solve1 . parse

run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input17"

parse = parseBlocks . splitOn [""] . lines

parseBlocks (registerBlock:(programStr:_):_) = (parseRegisters registerBlock, parseProgram programStr)

parseRegisters (a:b:c:_) = Registers (parseRegister a) (parseRegister b) (parseRegister c)

parseRegister = read . last . words

parseProgram = map read . splitOn "," . last . words

solve1 (registers, program) = intercalate "," $ map show $ executeProgram 0 registers program

executeProgram pointer registers program
    | pointer < length program  - 1 =
        let opcode = program !! pointer
            operand = program !! (pointer+1)
        in  doOpCode opcode operand pointer registers program
    | otherwise = []

doOpCode :: Int -> Int -> Int -> Registers -> [Int] -> [Int]
doOpCode 0 operand pointer registers@(Registers a b c) program =
    let val = combo operand registers
        denominator = 2^val
        result = a `div` denominator
    in  executeProgram (pointer+2) (Registers result b c) program
doOpCode 1 operand pointer registers@(Registers a b c) program =
    let result = b .^. operand
    in  executeProgram (pointer+2) (Registers a result c) program
doOpCode 2 operand pointer registers@(Registers a b c) program =
    let result = (combo operand registers) `mod` 8
    in  executeProgram (pointer+2) (Registers a result c) program
doOpCode 3 operand pointer registers@(Registers 0 b c) program = executeProgram (pointer+2) registers program
doOpCode 3 operand pointer registers@(Registers a b c) program = executeProgram operand registers program
doOpCode 4 operand pointer registers@(Registers a b c) program =
    let result = b .^. c
    in  executeProgram (pointer+2) (Registers a result c) program
doOpCode 5 operand pointer registers@(Registers a b c) program = ((combo operand registers) `mod` 8) : executeProgram (pointer+2) registers program
doOpCode 6 operand pointer registers@(Registers a b c) program =
    let result = a `div` (2^(combo operand registers))
    in  executeProgram (pointer+2) (Registers a result c) program
doOpCode 7 operand pointer registers@(Registers a b c) program =
    let result = a `div` (2^(combo operand registers))
    in  executeProgram (pointer+2) (Registers a b result) program
doOpCode _ operand pointer registers@(Registers a b c) program = error "bad opcode"

combo 0 _ = 0
combo 1 _ = 1
combo 2 _ = 2
combo 3 _ = 3
combo 4 (Registers a _ _) = a
combo 5 (Registers _ b _) = b
combo 6 (Registers _ _ c) = c
combo 7 _ = error "Combo 7 not allowed"

solve2 (_, program) = findDigit 0 targets program
    where targets = tail $ reverse $ tails program 

findDigit acc [] _ = [acc]
findDigit acc (target:targets) program = 
    let results = filter ((== target) .  (\a -> executeProgram 0 (Registers a 0 0) program)) (map (+(acc*8)) [0..7])
    in  concatMap (\acc' -> findDigit acc' targets program) results