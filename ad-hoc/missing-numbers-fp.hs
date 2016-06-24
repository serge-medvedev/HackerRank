-- https://www.hackerrank.com/challenges/missing-numbers-fp

import Control.Monad
import qualified Data.Map as M

findMissingNumbers [] [] = []
findMissingNumbers a b = [x | (x,_) <- M.toList $ M.differenceWith (\l r -> if l > r then Just l else Nothing) mB mA]
    where
        mA = M.fromListWith (+) [(iA, 1) | iA <- a]
        mB = M.fromListWith (+) [(iB, 1) | iB <- b]

main = do
    input <- getLine
    
    let n = (read :: String -> Int) input
    
    input <- getLine
    
    let a = [(read :: String -> Int) w | w <- words input]
    
    input <- getLine
    
    let m = (read :: String -> Int) input
    
    input <- getLine
    
    let b = [(read :: String -> Int) w | w <- words input]
    
    let results = findMissingNumbers a b
    
    forM_ results $ \x -> do
        putStr $ (show x) ++ " "

