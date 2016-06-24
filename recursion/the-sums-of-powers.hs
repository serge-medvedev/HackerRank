-- https://www.hackerrank.com/challenges/functional-programming-the-sums-of-powers

import Data.List

calcWays x n = sum [calcMatches searchSubspace 0 | searchSubspace <- tails [1..boundary]]
    where
        calcMatches [] _ = 0
        calcMatches (v:vs) result
            | result' == x = 1
            | result' > x = 0
            | otherwise = calcMatches' vs result'
            where
                result' = (result+(v^n))
                calcMatches' [] _ = 0
                calcMatches' ws result = (calcMatches ws result) + (calcMatches' (tail ws) result)
        boundary =
            case (findIndex (\k -> (k^n) > x) [1..x]) of
                Just value -> value
                Nothing -> x-1

main = do
    input <- getLine
    
    let x = (read :: String -> Int) input
    
    input <- getLine
    
    let n = (read :: String -> Int) input
    
    putStrLn $ show $ calcWays x n

