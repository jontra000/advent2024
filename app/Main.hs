module Main (main) where

import Data.Time
import P24

main :: IO ()
main = do
    input <- readFile inputLocation
    let result1 = run1 input
        result2 = run2 input
    start1 <- getCurrentTime
    print result1
    end1 <- getCurrentTime
    let diff1 = diffUTCTime end1 start1
    putStrLn $ "Execution time: " ++ show diff1
    start2 <- getCurrentTime
    print result2
    end2 <- getCurrentTime
    let diff2 = diffUTCTime end2 start2
    putStrLn $ "Execution time: " ++ show diff2