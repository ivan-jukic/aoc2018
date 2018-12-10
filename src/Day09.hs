module Day09 (main) where

import           Data.Foldable (foldl')
import           Debug.Trace   (trace, traceId, traceShowId)


data Player = Player { pid :: Int
                     , score :: Int
                     } deriving (Show)


main :: IO ()
main = do
    {-- }
    putStrLn . ("9 12 (32): " ++) . show $ playMarblesRec 9 25
    putStrLn . ("13 7999 (146373): " ++) . show $ playMarblesRec 13 7999
    putStrLn . ("17 1104 (2764): " ++) . show $ playMarblesRec 17 1104
    putStrLn . ("21 6111 (54718): " ++) . show $ playMarblesRec 21 6111
    putStrLn . ("30 5807 (37305): " ++) . show $ playMarblesRec 30 5807
    --}
    --putStrLn . ("Part 1: " ++) . show $ playMarblesRec 458 72019 -- ~ 1min 40sec
    putStrLn . ("Part 2: " ++) . show $ playMarblesRec 458 7201900 -- stack overflow :(


-- Part 1 - Play


-- Recursive solution

playMarblesRec :: Int -> Int -> Player
playMarblesRec numPlayers maxMoves = play' [0] 0 1 $ getPlayers numPlayers
    where play' marbles current move all@(player:xp)
            | move > maxMoves = getWinner all
            | otherwise = 
                if move `mod` 23 == 0 then
                    let (m, c, s) = getScore marbles current move
                        newScore = score player + s
                        player' = player { score = newScore }
                    in play' m c (move + 1) (xp ++ [player'])

                else let (m, c) = addMove marbles current move
                    in play' m c (move + 1) (xp ++ [player])
 

-- Initialize players

getPlayers :: Int -> [Player]
getPlayers no = map (\p -> Player p 0) $ [ 1 .. no ]


-- Get highest score

getWinner :: [Player] -> Player
getWinner []          = error "no players, no winner"
getWinner (player:xp) = foldl' (\p p' -> if score p' > score p then p' else p) player xp


-- get score for current marbles

getScore :: [Int] -> Int -> Int -> ([Int], Int, Int)
getScore marbles current move =
    let idx = current - 7
        removeIdx = if idx < 0 then length marbles + idx else idx
        removedValue = marbles !! removeIdx
        score = move + removedValue
    in (removeAt removeIdx marbles, removeIdx, score)


-- add marble

addMove :: [Int] -> Int -> Int -> ([Int], Int)
addMove marbles current move =
    let len = length marbles
        newIdx = (current + 2) `mod` len
    in  if newIdx == 0 then
            (marbles ++ [move], len)
        else if newIdx < len then
            (insertAt newIdx move marbles, newIdx)
        else
            error "cannot add marble to that index"


-- Insert into list at idx value val

insertAt :: Int -> Int -> [Int] -> [Int]
insertAt idx val list = take idx list ++ [val] ++ drop idx list


removeAt :: Int -> [Int] -> [Int]
removeAt idx list = take idx list ++ drop (idx + 1) list


-- My trace fns


myTrace :: Show a => String -> a -> a
myTrace msg val =
    trace ((msg ++) . show $ val ) val


myTraceLn :: Show a => String -> a -> a
myTraceLn msg val =
    trace (msg ++ (show val) ++ "\n") val
