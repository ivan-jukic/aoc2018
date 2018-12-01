module Day01 (main) where

import Data.Maybe


main :: IO ()
main = do
    contents <- readFile "inputs/day01"
    let numbers = parseInput contents
    putStrLn . ("Resulting frequency: " ++) $ getFrequency numbers
    let numbersCycle = cycle numbers
    putStrLn . ("First repeating frequency: " ++) $ getRepeatFreq [] 0 numbersCycle -- this one takes a while

    
getFrequency :: [Int] -> String
getFrequency = show . sum


getRepeatFreq :: [Int] -> Int -> [Int] -> String
getRepeatFreq prev freq (n:xn) =
    let newFreq = freq + n
    in if newFreq `elem` prev then
           show newFreq
       else
           getRepeatFreq ( prev ++ [ newFreq ] ) newFreq xn


parseInput :: String -> [Int]
parseInput = map readInt . lines


readInt :: String -> Int
readInt ('+':s') = read s' -- those pesky '+' made me chase my tail
readInt s = read s
