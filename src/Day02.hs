module Day02 (main) where

import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Text       as T


main :: IO ()
main = do
    contents <- readFile "inputs/day02"
    let ids = parseInput contents
    putStrLn . ("Checksum: " ++) . show . getChecksum $ ids
    putStrLn . ("Difference: " ++) . processResult . findLeastDifferent $ ids


-- Part 1


parseInput ::  String -> [String]
parseInput = map T.unpack . map (T.strip . T.pack) . lines -- this smells a bit


getChecksum :: [String] -> Int
getChecksum = product . Map.elems . mapOccurences . concat . map countCharOccurences


countCharOccurences :: String -> [Int]
countCharOccurences = unique . filter (\v -> v > 1) . Map.elems . mapOccurences


mapOccurences :: Ord a => [a] -> Map.Map a Int
mapOccurences = foldl (\cnt ch -> Map.insertWith (+) ch 1 cnt ) Map.empty


unique :: [Int] -> [Int]
unique = foldl (\unq val -> if val `elem` unq then unq else (unq ++ [val])) [] -- could this be done with list comprehensions?


-- Part 2


findLeastDifferent :: [String] -> Maybe (String, String)
findLeastDifferent []     = Nothing
findLeastDifferent (s:xs) =
    let solution = compareIDs s xs
    in if solution /= Nothing then solution else findLeastDifferent xs


compareIDs :: String -> [String] -> Maybe (String, String)
compareIDs _ []          = Nothing
compareIDs needle (s:xs) = if difference needle s == 1 then Just (needle, s) else compareIDs needle xs


-- Find out how many different letters there are at same positions in two strings
difference :: String -> String -> Int
difference a b = sum $ zipWith (\ca cb -> if ca == cb then 0 else 1) a b


processResult :: Maybe (String, String) -> String
processResult Nothing       = "n/a"
processResult (Just (a, b)) = expelIntruder a b ""


-- only keep the same letters at same places in the strings
expelIntruder :: String -> String -> String -> String
expelIntruder [] _ res = res
expelIntruder _ [] res = res
expelIntruder (a:xa) (b:xb) res = expelIntruder xa xb $ res ++ (if a == b then [a] else [])
