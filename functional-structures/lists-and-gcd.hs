-- https://www.hackerrank.com/challenges/lists-and-gcd

import Control.Monad
import Data.List
import Data.Ord

readLists q = 
    forM [1..q] $ \_ -> do
        input <- getLine
        return [(read :: String -> Int) w | w <- words input]

processLists [] = []
processLists lists = [minimumBy (comparing snd) g | g <- groupedItems, (length g) == n]
    where
        groupedItems = groupBy (\(a,_) (b,_) -> a == b) (sortBy (comparing fst) (concat normalizedLists))
        normalizedLists = [normalizeList l | l <- lists]
            where
                normalizeList [] = []
                normalizeList (x:y:other) = (x,y):(normalizeList other)
        n = length lists

main = do
    input <- getLine
    
    let q = (read :: String -> Int) input
    
    lists <- readLists q

    let result = processLists lists
    
    forM_ result $ \(x,p) -> do
        putStr $ (show x) ++ " " ++ (show p) ++ " "

    return ()

