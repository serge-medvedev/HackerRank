-- https://www.hackerrank.com/challenges/fp-sum-of-odd-elements

f arr = sum [a | a <- arr, odd a ]

main = do
   inputdata <- getContents
   putStrLn $ show $ f $ map (read :: String -> Int) $ lines inputdata

