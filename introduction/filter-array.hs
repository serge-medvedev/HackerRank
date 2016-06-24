-- https://www.hackerrank.com/challenges/fp-filter-array

f :: Int -> [Int] -> [Int]
f n arr = [a | a <- arr, a < n]

main = do 
    n <- readLn :: IO Int 
    inputdata <- getContents 
    let 
        numbers = map read (lines inputdata) :: [Int] 
    putStrLn . unlines $ (map show . f n) numbers

