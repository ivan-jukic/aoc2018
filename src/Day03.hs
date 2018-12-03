module Day03 (main) where

import           Text.Regex
import           Data.Maybe
import qualified Data.Map.Strict    as Map
import           Debug.Trace        (traceShowId)


data Rect = Rect { id' :: Int
                 , dx  :: Int
                 , dy  :: Int
                 , w   :: Int
                 , h   :: Int
                 } deriving (Show)


main :: IO ()
main = do
    contents <- readFile $ "inputs/_test_"
    let rects = mapMaybe id . map mapToRect . lines $ contents
    --putStrLn . ("Overlapping points: " ++) . show . Map.size . calculatePartOne $ rects
    putStrLn . ("Non overlapping rect: " ++) .show . calculatePartTwo $ rects

-- Part 2

calculatePartTwo :: [Rect] -> Map.Map String [Int]
calculatePartTwo rect =
    let idMap = getIdMap Map.empty rect
        overlappingIds = foldl (\ids ov -> if length ov > 1 then ids ++ ov else ov) [] & Map.elems


getIdMap :: Map.Map String [Int] -> [Rect] -> Map.Map String [Int]
getIdMap res []        = res
getIdMap res (rect:xr) =
    let points = getRectPoints rect
        updatedRes = foldl (\d k -> Map.insertWith (++) k [id' rect] d) res $ points
    in getIdMap updatedRes xr


-- Part 1

calculatePartOne :: [Rect] -> Map.Map String Int
calculatePartOne rects = 
    Map.filter (> 1) . getPointMap Map.empty $ rects


getPointMap :: Map.Map String Int -> [Rect] -> Map.Map String Int
getPointMap res []        = res
getPointMap res (rect:xr) =
    let points = getRectPoints rect
        updatedRes = foldl (\d k -> Map.insertWith (+) k 1 d) res $ points
    in getPointMap updatedRes xr


-- I can reuse these !!


-- Parse input into a type
mapToRect :: String -> Maybe Rect
mapToRect s =
    let rgx = mkRegex "^#([0-9]+)\\s+@\\s+([0-9]+),([0-9]+):\\s+([0-9]+)x([0-9]+)\\s*$"
        matches = matchRegex rgx s
    in case matches of
            Just (id:dx:dy:w:h:_) ->
                Just $ Rect { id' = read id, dx = read dx, dy = read dy, w = read w, h = read h }

            _ ->
                Nothing


getRectPoints :: Rect -> [String]
getRectPoints rect =
    let xpts = [ dx rect .. dx rect + w rect - 1 ]
        ypts = [ dy rect .. dy rect + h rect - 1 ]
    in map (\(x, y) -> show x ++ ":" ++ show y) [ (x, y) | x <- xpts, y <- ypts ]
