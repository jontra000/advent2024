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
solve1 (Input registers program) = intercalate "," $ map show $ executeProgram program registers

doOpCode :: Int -> Int -> Registers -> Registers
doOpCode 0 operand registers@(Registers a b c) =
    let val = combo operand registers
        denominator = 2^val
        result = a `div` denominator
    in  Registers result b c
doOpCode 1 operand (Registers a b c) =
    let result = b .^. operand
    in  Registers a result c
doOpCode 2 operand registers@(Registers a _ c) =
    let result = combo operand registers `mod` 8
    in  Registers a result c
doOpCode 4 _ (Registers a b c) =
    let result = b .^. c
    in  Registers a result c
doOpCode 6 operand registers@(Registers a _ c) =
    let result = a `div` (2^combo operand registers)
    in  Registers a result c
doOpCode 7 operand registers@(Registers a b _) =
    let result = a `div` (2^combo operand registers)
    in  Registers a b result
doOpCode x _ _ = error ("bad opcode: " ++ show x)

executeProgram :: Program -> Registers -> [Int]
executeProgram program registers = go registers program
    where go registers'@(Registers 0 _ _) (3:_:program') = go registers' program'
          go registers' (3:lit:_) = go registers' (drop lit program)
          go registers' (5:operand:program') = (combo operand registers' `mod` 8) : go registers' program'
          go registers' (opcode:operand:program') = go (doOpCode opcode operand registers') program'
          go _ _ = []

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
    let nextAs = map (+(acc*8)) [0..7]
        results = filter ((== target) .  executeProgram program . setA) nextAs
    in  concatMap (findDigit targets program) results

setA :: Int -> Registers
setA a = Registers a 0 0