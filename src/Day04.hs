module Day04 (main) where

import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import           Data.Foldable
import           Data.Ord
import           Debug.Trace     (traceShowId)
import           Text.Regex


data GuardAction
        = StartedShift Int
        | FellAsleep Int
        | WokeUp Int
        | Unknown
        deriving (Eq, Show)


-- not really a fan of the solution's code, think it can be better


main :: IO ()
main = do
    contents <- readFile "inputs/day04"
    let sorted = Map.toList $ sortedInput contents
        actionsOnly = map snd sorted
        sleeping = Map.toList $ sleepyGuards actionsOnly (-1) (-1) Map.empty
        (gid, min) = sleepiestGuard sleeping
        (gid', min', cnt') = mostSameMinsSlept sleeping
    putStrLn ("Sleepiest guard " ++ (show gid) ++ ", and min " ++ (show min) ++ ", with product " ++ (show $ gid * min))
    putStrLn ("Most same minute slept guard " ++ (show gid') ++ ", and min " ++ (show min') ++ ", with product " ++ (show $ gid' * min'))

-- Part 2

mostSameMinsSlept :: [(Int, [Int])] -> (Int, Int, Int)
mostSameMinsSlept sleepList =
    foldl findMostSleptMin (-1, -1, -1) sleepList


findMostSleptMin :: (Int, Int, Int) -> (Int, [Int]) -> (Int, Int, Int)
findMostSleptMin current@(gid, mins, cnt) (gid', minsSlept) =
        let minsCount = countSleepMins minsSlept
            (mins', cnt') = maximumBy compareSnd minsCount
        in if cnt' > cnt then (gid', mins', cnt') else current


-- Part 1


sleepiestGuard :: [(Int, [Int])] -> (Int, Int)
sleepiestGuard sleepList =
    let (gid, mins) = maximumBy compareSndList sleepList
        sleepyMins = countSleepMins mins
        (sleepiestMin, _) = maximumBy compareSnd sleepyMins
    in (gid, sleepiestMin)


countSleepMins :: [Int] -> [(Int, Int)]
countSleepMins mins = Map.toList . foldl (\res m -> Map.insertWith (+) m 1 res) Map.empty $ mins


compareSndList :: (Int, [Int]) -> (Int, [Int]) -> Ordering
compareSndList (g, m) (g', m') = compare (length m) (length m')


compareSnd :: Ord a => (t, a) -> (t, a) -> Ordering
compareSnd (m, c) (m', c') = compare c c'


-- Sort guards by sleeping minutes


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
