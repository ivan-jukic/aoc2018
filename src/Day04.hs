module Day04 (main) where

import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import           Debug.Trace     (traceShowId)
import           Text.Regex


data GuardAction
        = StartedShift Int
        | FellAsleep Int
        | WokeUp Int
        | Unknown
        deriving (Eq, Show)


data Guard = Guard { id'   :: Int
                   , sleep :: [Int]
                   } deriving (Show)


main :: IO ()
main = do
    contents <- readFile "inputs/day04"
    let sorted = Map.toList $ sortedInput contents
        actionsOnly = map snd sorted
        sleeping = sleepyGuards actionsOnly (-1) (-1) Map.empty
        (gid, min) = sleepiestGuard sleeping
    putStrLn ("Sleepiest guard " ++ (show gid) ++ ", and min " ++ (show min) ++ ", with total " ++ (show $ gid * min))


-- Part 1

sleepyGuards :: [GuardAction] -> Int -> Int -> Map.Map Int [Int] -> Map.Map Int [Int]
sleepyGuards [] _ _ res            = res
sleepyGuards (act:xa) gid smin res =
    case act of
        StartedShift newGid ->
            sleepyGuards xa newGid smin res
        
        FellAsleep newSmin->
            sleepyGuards xa gid newSmin res

        WokeUp wmin ->
            let newRes = Map.insertWith (++) gid [smin..(wmin - 1)] $ res
            in sleepyGuards xa gid smin newRes

        _ ->
            Map.empty -- error


sleepiestGuard :: Map.Map Int [Int] -> (Int, Int)
sleepiestGuard sleepMap =
    let (gid, mins) = foldl (\(g, m) (g', m') -> if length m' > length m then (g', m') else (g, m)) (-1, []) $ Map.toList sleepMap
        sleepyMins = Map.toList . foldl (\res m -> Map.insertWith (+) m 1 res) Map.empty $ mins
        (sleepiesMin, _) = foldl (\(m, c) (m', c') -> if c' > c then (m', c') else (m, c)) (-1, -1) sleepyMins
    in (gid, sleepiesMin)


-- Sort input

sortedInput :: String -> Map.Map String GuardAction
sortedInput contents = foldl (\m (k, a) -> Map.insert k a m) Map.empty . map parseString . inputLines $ contents


parseString :: String -> (String, GuardAction)
parseString inpt =
    let datetime = take 16 . tail $ inpt
        min = read . drop 14 $ datetime
        fixedInput = drop 18 inpt
    in ( datetime
       , if contains' sleepRgx fixedInput then
            FellAsleep min
         else if contains' wakeUpRgx fixedInput then
            WokeUp min
         else
            matchShiftBegins fixedInput min
       )


matchShiftBegins :: String -> Int -> GuardAction
matchShiftBegins inpt min =
    case matchRegex shiftRgx inpt of
        Just (gid:_) ->
            StartedShift . read $ gid
        _ ->
            Unknown


contains' :: Regex -> String -> Bool
contains' rgx haystack = matchRegex rgx haystack /= Nothing


shiftRgx :: Regex
shiftRgx =
    mkRegex "^\\s*Guard #([0-9]+) begins shift\\s*$"


sleepRgx :: Regex
sleepRgx =
    mkRegex "^\\s*falls asleep\\s*$"


wakeUpRgx :: Regex
wakeUpRgx =
    mkRegex "^\\s*wakes up\\s*$"


-- Separates input per lines
inputLines ::  String -> [String]
inputLines = map T.unpack . map (T.strip . T.pack) . lines


-- testing
hasErrors :: Map.Map String GuardAction -> Bool
hasErrors mappedData =
    foldl (\b (d, a) -> if a == Unknown then True else b) False . Map.toList $ mappedData
