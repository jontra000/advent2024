module P17 (run1, run2, inputLocation) where

import Data.Bits
import Data.List.Split (splitOn)
import Data.List (intercalate, tails)

data Registers = Registers Int Int Int deriving Show
type Program = [Int]
data Input = Input Registers Program

run1 :: String -> String
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input17"

parse :: String -> Input
parse = parseBlocks . splitOn [""] . lines

parseBlocks :: [[String]] -> Input
parseBlocks (registerBlock:(programStr:_):_) = Input (parseRegisters registerBlock) (parseProgram programStr)
parseBlocks e = error ("Bad input: " ++ show e)

parseRegisters :: [String] -> Registers
parseRegisters (a:b:c:_) = Registers (parseRegister a) (parseRegister b) (parseRegister c)
parseRegisters e = error ("Bad input: " ++ show e)

parseRegister :: String -> Int
parseRegister = read . last . words

parseProgram :: String -> Program
parseProgram = map read . splitOn "," . last . words

solve1 :: Input -> String
solve1 (Input registers program) = intercalate "," $ map show $ executeProgram 0 registers program

executeProgram :: Int -> Registers -> Program -> [Int]
executeProgram pointer registers program
    | pointer < length program  - 1 =
        let opcode = program !! pointer
            operand = program !! (pointer+1)
        in  doOpCode opcode operand pointer registers program
    | otherwise = []

doOpCode :: Int -> Int -> Int -> Registers -> Program -> [Int]
doOpCode 0 operand pointer registers@(Registers a b c) program =
    let val = combo operand registers
        denominator = 2^val
        result = a `div` denominator
    in  executeProgram (pointer+2) (Registers result b c) program
doOpCode 1 operand pointer (Registers a b c) program =
    let result = b .^. operand
    in  executeProgram (pointer+2) (Registers a result c) program
doOpCode 2 operand pointer registers@(Registers a _ c) program =
    let result = combo operand registers `mod` 8
    in  executeProgram (pointer+2) (Registers a result c) program
doOpCode 3 _ pointer registers@(Registers 0 _ _) program = executeProgram (pointer+2) registers program
doOpCode 3 operand _ registers program = executeProgram operand registers program
doOpCode 4 _ pointer (Registers a b c) program =
    let result = b .^. c
    in  executeProgram (pointer+2) (Registers a result c) program
doOpCode 5 operand pointer registers program = (combo operand registers `mod` 8) : executeProgram (pointer+2) registers program
doOpCode 6 operand pointer registers@(Registers a _ c) program =
    let result = a `div` (2^combo operand registers)
    in  executeProgram (pointer+2) (Registers a result c) program
doOpCode 7 operand pointer registers@(Registers a b _) program =
    let result = a `div` (2^combo operand registers)
    in  executeProgram (pointer+2) (Registers a b result) program
doOpCode x _ _ _ _ = error ("bad opcode: " ++ show x)

combo :: Int -> Registers -> Int
combo 0 _ = 0
combo 1 _ = 1
combo 2 _ = 2
combo 3 _ = 3
combo 4 (Registers a _ _) = a
combo 5 (Registers _ b _) = b
combo 6 (Registers _ _ c) = c
combo x _ = error ("Bad combo arg: " ++ show x)

solve2 :: Input -> Int
solve2 (Input _ program) = minimum $ findDigit targets program 0
    where targets = tail $ reverse $ tails program

findDigit :: [[Int]] -> Program -> Int -> [Int]
findDigit [] _ acc = [acc]
findDigit (target:targets) program acc =
    let results = filter ((== target) .  (\a -> executeProgram 0 (Registers a 0 0) program)) (map (+(acc*8)) [0..7])
    in  concatMap (findDigit targets program) results