{-# Language GeneralizedNewtypeDeriving #-}

module Main (main) where

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Foldable   (foldl')
import           Data.List       (maximumBy)
import qualified Common          as C


data Cell = Cell { x :: Int
                 , y :: Int
                 } deriving (Show, Eq, Ord)


newtype Grid = Grid (Map Cell Int) deriving (Show, Semigroup, Monoid)


newtype SerialNo = SerialNo Int


newtype Dim = Dim Int


newtype SubDim = SubDim Int deriving (Show)


gridSize :: Dim
gridSize = Dim 300


gridSerial :: SerialNo
gridSerial = SerialNo 1718


minSubGrid :: Int
minSubGrid = 10


main :: IO ()
main = do
    putStrLn . ("Max (3x3) power: " ++) . show $ calcMaxPower gridSize (SubDim 3) gridSerial
    putStrLn . ("Max absolute power: " ++) . show $ absolutePower gridSize gridSerial -- takes a while to go through grids


-- Pt 2


absolutePower :: Dim -> SerialNo -> (Cell, Int, SubDim)
absolutePower dim@(Dim d) serialNo =
    let subDims = [ SubDim s | s <- [ minSubGrid .. 20] ] -- not necessary to go through all of the possible sub grid dimensions
    in findAbsolutePower $ map (\sd -> (\(c, p) -> (c, p, sd)) $ calcMaxPower dim sd serialNo) subDims


findAbsolutePower :: [(Cell, Int, SubDim)] -> (Cell, Int, SubDim)
findAbsolutePower []     = error "no absolute powers :("
findAbsolutePower (c:xc) = foldl' (\pmax@(_, p, _) pc@(_, p', _) -> if p' > p then pc else pmax) c xc


-- Pt 1


calcMaxPower :: Dim -> SubDim -> SerialNo -> (Cell, Int)
calcMaxPower dim@(Dim dim') subDim@(SubDim sd) serialNo =
    let rng = [ 1 .. dim' - sd + 1 ]
    in findMaxPower $ calc' rng rng []
    where calc' [] _ res          = res
          calc' (x:xp) yrange res =
            let newRes = map (calcSubGridPower subDim serialNo) $ [ Cell x y | y <- yrange ]
            in calc' xp yrange (res ++ newRes)


findMaxPower :: [(Cell, Int)] -> (Cell,Int)
findMaxPower []     = error "no powers :("
findMaxPower (c:xc) = foldl' (\m@(cmax, pmax) (c, p) -> if p > pmax then (c, p) else m) c xc


calcSubGridPower :: SubDim -> SerialNo -> Cell -> (Cell, Int)
calcSubGridPower (SubDim subDim) serialNo cell@(Cell x' y') =
    (cell, sum . map (calcCellPower serialNo) $ [ Cell x y | x <- [ x' .. x' + subDim - 1 ], y <- [ y' .. y' + subDim - 1 ] ])


calcCellPower :: SerialNo -> Cell -> Int
calcCellPower (SerialNo serialNo) cell =
    let rackID = x cell + 10
        powerLevelStart = rackID * y cell
        increasedPowerLevel = (powerLevelStart + serialNo) * rackID
        hundredDigit = read . take 1 . drop 2 . reverse $ show increasedPowerLevel
    in hundredDigit - 5
