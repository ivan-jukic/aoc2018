module Day05 (main) where

import           Data.Char
import           Debug.Trace (traceShowId)


-- I feel a bit better about this code


main :: IO ()
main = do
    contents <- readFile "inputs/day05"
    putStrLn . ( "Lenght after reduce: " ++) . show . length $ reducePolymer contents -- takes 1-2 sec
    putStrLn . ( "Min compressed lenght: " ++) . show $ shortestPolymer contents -- ~ 30 sec


-- Part 2


shortestPolymer :: String -> Int
shortestPolymer [] = error "no input"
shortestPolymer inpt = minimum . map snd . map (removeAndReduce inpt) . unique . map toLower $ inpt


removeAndReduce :: String -> Char -> (Char, Int)
removeAndReduce inpt ch =
    let filtered = filter (\c -> toLower c /= ch) inpt
    in ( ch, length $ reducePolymer filtered )


-- Part 1


reducePolymer :: String -> String
reducePolymer []      = error "everything reduced"
reducePolymer polymer = reduce' [] polymer
    where reduce' res []  = res
          reduce' res [p] = res ++ [p] -- when reduced to the last element
          reduce' res (a:b:xp)
            | shouldCollapse a b =
                if shouldCollapseWithPrev res xp then
                    let prev = last res
                    in reduce' (init res) ([prev] ++ xp)
                else
                    reduce' res xp
            | otherwise         = reduce' (res ++ [a]) ([b] ++ xp)


shouldCollapseWithPrev :: String -> String -> Bool
shouldCollapseWithPrev [] _       = False
shouldCollapseWithPrev _ []       = False
shouldCollapseWithPrev prev (p:_) =
    let p' = last prev
    in shouldCollapse p p'


shouldCollapse :: Char -> Char -> Bool
shouldCollapse a b = ((toUpper a) == b || a == (toUpper b)) && a /= b


--


unique :: String -> String
unique = foldl (\unq val -> if val `elem` unq then unq else (unq ++ [val])) []
