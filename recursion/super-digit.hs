-- https://www.hackerrank.com/challenges/super-digit

numToDigits :: Integer -> [Integer]
numToDigits x | x < 10 = [x] | otherwise = (x `mod` 10):(numToDigits $ x `div` 10)

calcSuperDigit :: Integer -> Integer -> Integer
calcSuperDigit n k = calcSuperDigit' $ (calcSuperDigit' n ) * (calcSuperDigit' k)
    where
        calcSuperDigit' x | x == x' = x | otherwise = calcSuperDigit' x'
            where
                x' = sum $ numToDigits x

main = do
    (n:k:_) <- fmap (map read . words) getLine :: IO [Integer]
    
    putStrLn $ show $ calcSuperDigit n k

