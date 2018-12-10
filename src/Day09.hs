module Day09 (main) where

import           Data.Foldable (foldl', toList)
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq, (|>))


data Player = Player { pid :: Int
                     , score :: Int
                     } deriving (Show)


data State = State { marbles :: Seq Int
                   , current :: Int
                   , players :: Seq Player
                   } deriving (Show)


main :: IO ()
main = do
    putStrLn . ("Part 1: " ++) . show $ playMarbles 458 72019 -- ~ 2sec with Sequences vs 1m 40s with Lists
    putStrLn . ("Part 2: " ++) . show $ playMarbles 458 7201900 -- ? stack overflow ? :(


initState :: Int -> State
initState numPlayers =
    State { marbles = Seq.singleton 0
          , current = 0
          , players = getPlayers numPlayers
          }


getPlayers :: Int -> Seq Player
getPlayers no = foldl' (\s p -> s |> Player p 0) Seq.empty $ [ 1 .. no ]


getWinner :: Seq Player -> Int
getWinner players = maximum . map score $ toList players


-- Play


playMarbles :: Int -> Int -> Int
playMarbles maxPlayers maxMoves = getWinner . players $ foldl' play' (initState maxPlayers) [1..maxMoves]
    where play' s move
            | move `mod` 23 == 0 = onePlay move s
            | otherwise          = addMove move s


onePlay :: Int -> State -> State
onePlay move s@(State marbles current _) =
    let idx = (current - 7) `mod` Seq.length marbles
        moveValue = move + (Seq.index marbles idx)
    in rotatePlayers moveValue $
        s { marbles = Seq.deleteAt idx marbles
          , current = idx
          }


addMove :: Int -> State -> State
addMove move state@(State marbles current _) =
    let idx = (current + 1) `mod` Seq.length marbles + 1
    in rotatePlayers 0 $ state { marbles = Seq.insertAt idx move marbles, current = idx }


rotatePlayers :: Int -> State -> State
rotatePlayers addMoveScore state =
    let player = Seq.index (players state) 0
        player' = player { score = (score player) + addMoveScore }
        players' = Seq.drop 1 $ players state
    in state { players = players' |> player' }
