module Day06 (main) where

import           Data.Foldable   (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import           Debug.Trace     (traceShowId)
import           Text.Regex


data Point = Point { x :: Int
                   , y :: Int
                   } deriving (Show, Ord, Eq)


main :: IO ()
main = do
    contents <- readFile "inputs/day06"
    let points = parseInput contents
        coords = getBoundingCoords points
        distances = mapDistances coords points
        infinitePoints = infiniteAreaPoints coords distances
    putStrLn . ("Max bounded area is: " ++ ) . show . calcMaxArea coords infinitePoints $ distances -- ~ 20-22 sec
    putStrLn . ("Region size: " ++) . show . length $ calcRegions coords points -- ~ 5-6 sec


-- Part 2


distLimit :: Int
distLimit = 10000


calcRegions :: (Point, Point) -> [Point] -> [Int]
calcRegions (pmin, pmax) points =
    let coords = [ Point x y | x <- [x pmin .. x pmax], y <- [y pmin .. y pmax]]
    in filter (\v -> v < distLimit) . map (calcAllDist points) $ coords


calcAllDist :: [Point] -> Point -> Int
calcAllDist points pt = foldl' (\total pt' -> total + (manhattanDist pt pt')) 0 points


-- Part 1 - probably not the nicest solution... 


calcMaxArea :: (Point, Point) -> [Point] -> Map.Map Point ([Point], Int) -> Int
calcMaxArea (pmin, pmax) infinitePoints distances =
    let areas = Map.toList . foldl' countCoords Map.empty $ Map.elems distances
        notInfinitePoint = \(p, _) -> p `notElem` infinitePoints
    in maximum . map snd . filter notInfinitePoint $ areas


countCoords :: Map.Map Point Int -> ([Point], Int) -> Map.Map Point Int
countCoords res ([p], _) = Map.insertWith (+) p 1 res
countCoords res _        = res


-- get rid of all the points that touch the edge of the bounding box


infiniteAreaPoints :: (Point, Point) -> Map.Map Point ([Point], Int) -> [Point]
infiniteAreaPoints (pmin, pmax) distances =
    let xrng   = [x pmin..x pmax]
        yrng   = [y pmin..y pmax]
        boundingPoints =
            map (\(x', y') -> Point x' y') $
                [ (x, y pmin) | x <- xrng ] ++
                [ (x, y pmax) | x <- xrng ] ++
                [ (x pmin, y) | y <- yrng ] ++
                [ (x pmax, y) | y <- yrng ]
    in unique . foldl'
        (\res key ->
            case Map.lookup key distances of
                Just ([pt], _) ->
                    res ++ [pt]

                _ ->
                    res
        )
        []
        $ boundingPoints


unique :: [Point] -> [Point]
unique = foldl (\unq val -> if val `elem` unq then unq else (unq ++ [val])) []


-- Create distances map


mapDistances :: (Point, Point) -> [Point] -> Map.Map Point ([Point], Int)
mapDistances _ []                = error "no points"
mapDistances (pmin, pmax) points =
    let xcoords = [ x pmin .. x pmax ]
        ycoords = [ y pmin .. y pmax ]
        coords  = [ (x, y) | x <- xcoords, y <- ycoords ]
    in createDistMap points coords


createDistMap :: [Point] -> [(Int, Int)] -> Map.Map Point ([Point], Int)
createDistMap [] _          = error "no points for map"
createDistMap _ []          = error "no coords"
createDistMap points coords = create' points Map.empty
    where create' [] res     = res
          create' (p:xp) res =
            let newRes = foldl' (checkCoordDistance p) res coords
            in create' xp newRes


checkCoordDistance :: Point -> Map.Map Point ([Point], Int) -> (Int, Int) -> Map.Map Point ([Point], Int)
checkCoordDistance p res (cx, cy) =
    let key = Point cx cy
        d = manhattanDist p key
    in case Map.lookup key res of
        Just (pts, d') ->
            if d < d' then
                Map.insert key ([p], d) res
            else if d == d' then
                Map.insert key (pts ++ [p], d') res -- when distances are same
            else
                res
        Nothing ->
            Map.insert key ([p], d) res


-- Get bounding rect


getBoundingCoords :: [Point] -> (Point, Point)
getBoundingCoords []     = error "no points"
getBoundingCoords [pt]   = error "only one point"
getBoundingCoords (p1:p2:points) = coords' (p1, p2) points
    where coords' (pmin, pmax) []     = (pmin, pmax)
          coords' (pmin, pmax) (p:xp) = coords' (compareCoords pmin pmax p) xp


compareCoords :: Point -> Point -> Point -> (Point, Point)
compareCoords pmin pmax p =
    let minx = if x p < x pmin then x p else x pmin
        miny = if y p < y pmin then y p else y pmin
        maxx = if x p > x pmax then x p else x pmax
        maxy = if y p > y pmax then y p else y pmax
    in ( Point { x = minx, y = miny }, Point { x = maxx, y = maxy } )


-- calc dist


manhattanDist :: Point -> Point -> Int
manhattanDist pt1 pt2 =
    abs(x pt1 - x pt2) + abs(y pt1 - y pt2)


-- Parse points


parseInput :: String -> [Point]
parseInput inpt = map getPoint . inputLines $ inpt


getPoint :: String -> Point
getPoint pt =
    let rgx = mkRegex "^([0-9]+),\\s+([0-9]+)$"
    in case matchRegex rgx pt of
        Just (x':y':_) ->
            Point { x = read x', y = read y' }

        _ ->
            error $ "point parse failed: " ++ pt


-- Separates input per lines

inputLines ::  String -> [String]
inputLines = map trim . lines


trim :: String -> String
trim = T.unpack . T.strip . T.pack
