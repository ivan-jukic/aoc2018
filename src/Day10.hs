module Main (main) where


import qualified Common          as C
import           Data.Foldable   (foldl')
import           Text.Regex
import qualified Data.Map.Strict as Map


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
    content <- parseInput <$> readFile "inputs/day10"
    putStrLn . movePoints $ content


-- pt 1


movePoints :: [Point] -> String
movePoints points = movePoints' points 1 False
    where movePoints' pts sec gridShowed =
            let pts' = map moveSinglePoint pts
                grid = pointsGrid pts'
            in if gridShowed && length grid == 0 then
                grid
            else
                (if length grid > 0 then "Sec " ++ (show sec) ++ "\n" else "")
                    ++ grid
                    ++ (movePoints' pts' (sec + 1) (length grid > 0))


moveSinglePoint :: Point -> Point
moveSinglePoint (Point (Position x y) vel@(Velocity vx vy)) = Point (Position (x + vx) (y + vy)) vel


-- printing points

pointDispersion :: Int
pointDispersion = 1000


pointsGrid :: [Point] -> String
pointsGrid points =
    let (minp, maxp) = getMinMaxPos points
        (xmin, ymin) = (x minp, y minp)
        (xmax, ymax) = (x maxp, y maxp)
        deltax = abs(xmax - xmin)
        deltay = abs(ymax - ymin)
    in if deltax * deltay < pointDispersion then
            let xrange = [xmin..xmax]
                yrange = [ymin..ymax]
                pts = [(x, y) | x <- xrange, y <- yrange]
                emptyMap = foldl' (\m (x', y') -> Map.insert (x', y') 0 m) Map.empty pts
                fullMap = foldl' (\m pt -> Map.insert (getPointPos pt) 1 m) emptyMap points
            in (showPoints xrange yrange fullMap) ++ "\n"
                
       else
            ""


showPoints :: [Int] -> [Int] -> Map.Map (Int, Int) Int -> String
showPoints _ [] _            = ""
showPoints xrange (y:xy) pts =
    let rowStr = "\t" ++ (foldl' (\s x -> (s ++) $ getGridPoint x y pts) "" xrange) ++ "\n"
    in (rowStr ++) $ showPoints xrange xy pts


getGridPoint :: Int -> Int -> Map.Map (Int, Int) Int -> String
getGridPoint x y pts = if pts Map.! (x, y) == 0 then "." else "#"


-- convenience


getMinMaxPos :: [Point] -> (Position, Position)
getMinMaxPos (p:pts) = foldl' comparePoints (pos p, pos p) pts


comparePoints :: (Position, Position) -> Point -> (Position, Position)
comparePoints ((Position xmin ymin), (Position xmax ymax)) p =
    let (x, y) = getPointPos p
    in ( Position (if x < xmin then x else xmin) (if y < ymin then y else ymin)
       , Position (if x > xmax then x else xmax) (if y > ymax then y else ymax)
       )


getPointPos :: Point -> (Int, Int)
getPointPos (Point (Position x y) _) = (x, y)


getPointVel :: Point -> (Int, Int)
getPointVel (Point _ (Velocity vx vy)) = (vx, vy)


-- parsing input


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
