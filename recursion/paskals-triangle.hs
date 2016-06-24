-- https://www.hackerrank.com/challenges/pascals-triangle

module Main where

pt :: Int -> [[Int]]
pt 0 = []
pt k = [[(product [1..n]) `div` ((product [1..r])*(product [1..(n-r)])) | r <- [0..n] ] | n <- [0..(k-1)]]

array_to_string :: [Int] -> String
array_to_string [] = ""
array_to_string (x:xs) = (show x) ++ " " ++ (array_to_string xs)

main = do
    input <- getLine
    let k = (read :: String -> Int) input
    let result = pt k
    mapM_ putStrLn ( map array_to_string result )

