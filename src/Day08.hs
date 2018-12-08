module Day08 (main) where

import      Data.Foldable (foldl')

data Tree = Empty | Leaf [Int] | Node [Int] [Tree] deriving (Show)


main :: IO ()
main = do
    content <- parseInput <$> readFile "inputs/day08"
    let (tree, _) = buildTree content
    putStrLn . ("Metadata entries sum: " ++) . show . sumMeta $ tree


-- Part 1


sumMeta :: Tree -> Int
sumMeta Empty       = 0
sumMeta (Leaf meta) = sum meta
sumMeta tree        = sumMeta' tree 0 
    where sumMeta' (Node meta children) total = total + (foldl' (\t c -> sumMeta' c t ) (sum meta) children)
          sumMeta' (Leaf meta) total          = total + (sum meta)


-- build tree

buildTree :: [Int] -> (Tree, [Int])
buildTree treeData = fillTree treeData (Empty, [])
    where fillTree [] tree = tree
          fillTree (numChild:numMeta:treeData') (tree, _)
            | numChild > 0 =
                let (children, newTreeData) = getChildNodes numChild treeData'
                in (nodeAddMeta (take numMeta newTreeData) . nodeAddChildren children $ tree, drop numMeta newTreeData)
            | otherwise   = ( Leaf $ take numMeta treeData', drop numMeta treeData' )


getChildNodes :: Int -> [Int] -> ([Tree], [Int])
getChildNodes numChild treeData =
    foldl'
        (\(children, td) _ ->
            let (child, td') = buildTree td
            in (children ++ [child], td')
        )
        ([], treeData)
        [ 1 .. numChild ]


nodeAddChildren :: [Tree] -> Tree -> Tree
nodeAddChildren children Empty                 = Node [0] children
nodeAddChildren _ (Leaf m)                     = Leaf m
nodeAddChildren children' (Node meta children) = Node meta (children ++ children') 


nodeAddMeta :: [Int] -> Tree -> Tree
nodeAddMeta _ Empty                = Empty
nodeAddMeta meta (Leaf _)          = Leaf meta
nodeAddMeta meta (Node _ children) = Node meta children


-- parsing input


parseInput :: String -> [Int]
parseInput = map read . words
