module Main (main) where

import qualified Common as C
import           Text.Regex


data Position = Position { x :: Int
                         , y :: Int
                         } deriving (Show)


data Velocity = Velocity { vx :: Int
                         , vy :: Int
                         } deriving (Show)


data Point = Point { pos :: Position
                   , vel :: Velocity
                   } deriving (Show)

main :: IO ()
main = do
    content <- parseInput <$> readFile "inputs/_test_"
    putStrLn . show $ content


parseInput :: String -> [Point]
parseInput inpt =
    map parseLine $ C.inputLines inpt


parseLine :: String -> Point
parseLine line =
    let rgx = mkRegex "^position=<\\s*([-0-9]+),\\s*([-0-9]+)>\\s*velocity=<\\s*([-0-9]+),\\s*([-0-9]+)>$"
    in case matchRegex rgx line of
        Just (x':y':vx':vy':_) ->
            Point (Position (read x') (read y')) (Velocity (read vx') (read vy'))

        _ ->
            error $ "Parse error: " ++ line
