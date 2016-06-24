-- https://www.hackerrank.com/challenges/filter-elements

import Data.List
import Data.Maybe
import Data.Ord
import Control.Monad

filterElements _ [] = [-1]
filterElements k aList
    | null result = [-1]
    | otherwise = result
    where
        result = [a | (a,_) <- sortBy (comparing snd) filteredItems]
        filteredItems = [(g !! 0, fromJust $ elemIndex (g !! 0) aList) | g <- group $ sort aList, (length g) >= k]

processTCases 0 = return ()
processTCases t = do
    input <- getLine
    
    let (n:k:_) = [(read :: String -> Int) w | w <- words input]
    
    input <- getLine
    
    let aList = [(read :: String -> Int) w | w <- words input]
    let result = filterElements k aList
    
    forM_ result $ \x -> do
        putStr $ (show x) ++ " "
    
    putStrLn ""
    
    processTCases (t-1)
    
main = do
    input <- getLine
    
    let t = (read :: String -> Int) input
    
    processTCases t

