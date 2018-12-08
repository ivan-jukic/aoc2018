{-# LANGUAGE OverloadedStrings #-}

module Day07 (main) where

import           Data.Foldable        (foldl')
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T
import           Debug.Trace          (traceShowId)
import qualified Data.List            as L
import qualified Data.Char            as C


main :: IO ()
main = do
    content <- map parseInput <$> lines <$> readFile "inputs/day07"
    --putStrLn . ("Steps order: " ++) . resolveDeps $ content
    putStrLn . ("Parallel time: " ++) . show $ parallelResolve content


-- Part 2

maxWorkers :: Int
maxWorkers = 5


baseDuration :: Int
baseDuration = 60


parallelResolve :: [(Char, Char)] -> Int
parallelResolve []   = error "No steps available"
parallelResolve deps = calcTime $ calcDeps deps


calcTime :: [(Char, String)] -> Int
calcTime deps = calc' deps "" [] 0
    where calc' [] _ _ total           = total
          calc' d resolved queue total =
            let q' = take maxWorkers . (queue ++) $ getAvailSteps d resolved
                d' = filterDeps d q'
                next = resolveNext q' -- next steps to be resolved
                nextTime = getMinTime next -- common time for those steps
                nextTotal = getNextTotal next -- total time for those next steps
                nextResolved = getNextResolved next -- sorted resolved next steps

                filteredQueue = filterQueue q' next -- filter queue with re to next
                newQueue = fixQueueTimes filteredQueue nextTime -- reduce times of the steps in the queue based on the resolved steps

            in calc' d' (resolved ++ nextResolved) newQueue (total + nextTotal)


fixQueueTimes :: [(Char, Int)] -> Int -> [(Char, Int)]
fixQueueTimes queue t = map (\(c, t') -> (c, t' - t)) queue


filterQueue :: [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
filterQueue queue next = foldl' (\r q -> if q `notElem` next then r ++ [q] else r) [] queue


getMinTime :: [(Char, Int)] -> Int
getMinTime []         = 0
getMinTime ((_, t):_) = t


getNextResolved :: [(Char, Int)] -> String
getNextResolved next = L.sort $ foldl' (\r (c, _) -> r ++ [c]) "" next


getNextTotal :: [(Char, Int)] -> Int
getNextTotal []   = 0
getNextTotal next = foldl' (\t (_, t') -> t + t') 0 next


resolveNext :: [(Char, Int)] -> [(Char, Int)]
resolveNext ((c, t):xq) = getQuickest xq t [(c, t)]


getQuickest :: [(Char, Int)] -> Int -> [(Char, Int)] -> [(Char, Int)]
getQuickest [] _ quickest           = quickest
getQuickest ((c, t):xq) minTime quickest =
    let (minTime', quickest') =
            if t < minTime then
                (t, [(c, t)])
            else if t == minTime then
                (minTime, quickest ++ [(c, t)])
            else
                (minTime, quickest)
    in getQuickest xq minTime' quickest'


-- Filter deps step if step is in the queue

filterDeps :: [(Char, String)] -> [(Char, Int)] -> [(Char, String)]
filterDeps deps queue =
    let q' = foldl' (\r (c, _) -> r ++ [c]) "" queue
    in filter (\(c, _) -> c `notElem` q') deps


-- Get all steps whose dependecies have been resolved, and return them
-- as tuples; step and time required to solve task

getAvailSteps :: [(Char, String)] -> String -> [(Char, Int)]
getAvailSteps deps resolved = avail' deps resolved ""
    where avail' [] _ res        = stepsToTime res
          avail' ((c, d):xd) r q
            | allDepsResolved d r = avail' xd r (q ++ [c])
            | otherwise           = avail' xd r q


stepsToTime :: String -> [(Char, Int)]
stepsToTime steps = charTime' steps []
    where charTime' "" res     = res
          charTime' (c:xc) res = charTime' xc (res ++ [(c, charToTime c)])


charToTime :: Char -> Int
charToTime c = (C.ord c) - (C.ord 'A') + baseDuration + 1


-- Part 1


resolveDeps :: [(Char, Char)] -> String
resolveDeps []   = error "No steps available"
resolveDeps deps =
    let deps' = calcDeps deps
    in resolve' deps' ""
    where resolve' [] resolved = resolved
          resolve' d resolved  =
            let resolved' = singleResolvePass d resolved
                d' = filterResolved d resolved'
            in resolve' d' resolved'


singleResolvePass :: [(Char, String)] -> String -> String
singleResolvePass d r = singleResolve d r ""
    where singleResolve [] resolved buffer             = (resolved ++) . take 1 $ L.sort buffer
          singleResolve ((c, deps):xd) resolved buffer =
            let depsResolved = allDepsResolved deps resolved
            in singleResolve xd resolved $ buffer ++ (if depsResolved then [c] else [])


-- Some common fns

allDepsResolved :: String -> String -> Bool
allDepsResolved deps resolved = foldl' (\b c -> if b then c `elem` resolved else False) True deps


filterResolved :: [(Char, String)] -> String -> [(Char, String)]
filterResolved deps resolved = filter (\(c, _) -> c `notElem` resolved) deps


calcDeps :: [(Char, Char)] -> [(Char, String)]
calcDeps deps =
    let initRes = foldl' (\m c -> Map.insert c "" m) Map.empty $ uniqueSteps deps
    in calc' deps initRes
    where calc' [] res         = Map.toList res
          calc' ((a,b):xd) res = calc' xd $ Map.insertWith (++) b [a] res


uniqueSteps :: [(Char, Char)] -> String
uniqueSteps []   = error "Need some steps man"
uniqueSteps deps = unique' deps []
    where unique' [] res          = res
          unique' ((a, b):xd) res = unique' xd . addIfNotElem b . addIfNotElem a $ res


addIfNotElem :: Char -> String -> String
addIfNotElem c s = if c `notElem` s then s ++ [c] else s


-- get input


parseInput :: String -> (Char, Char)
parseInput inpt =
    case Atto.parse lineParser $ T.pack inpt of
        Atto.Done _ r ->
            r

        Atto.Partial _ ->
            error "partial error"

        Atto.Fail _ ctxMsgs msg ->
            error msg


lineParser :: Parser (Char, Char)
lineParser = do
    _ <- Atto.string "Step "
    a <- Atto.satisfy isUpper
    _ <- Atto.string " must be finished before step "
    b <- Atto.satisfy isUpper
    pure $ (a, b)
    where isUpper c = c >= 'A' && c <= 'Z'
