-- https://www.hackerrank.com/challenges/captain-prime

import Control.Monad
import Data.List

numToDigits :: Integer -> [Integer]
numToDigits x = reverse $ numToDigits' x
    where
        numToDigits' x | x < 10 = [x] | otherwise = (x `mod` 10):(numToDigits' $ x `div` 10)

digitsToNum :: [Integer] -> Integer
digitsToNum [] = error "invalid input"
digitsToNum input = digitsToNum' input 0
    where
        digitsToNum' [] p = p
        digitsToNum' (d:ds) p | p == 0 = digitsToNum' ds d | otherwise = digitsToNum' ds (p * 10 + d)

findDivisors :: Integer -> [Integer]
findDivisors x = nub $ concat [[y, x `div` y] | y <- [1..(ceiling $ sqrt $ fromIntegral x)], (x `mod` y) == 0]

isPrime :: Integer -> Bool
isPrime x = (toInteger $ length $ findDivisors x) == 2

checkIfPrimes [] = True
checkIfPrimes (x:xs) = (isPrime x) && (checkIfPrimes xs)

captainPrime id
    | isCentral = "CENTRAL"
    | isLeft = "LEFT"
    | isRight = "RIGHT"
    | otherwise = "DEAD"
    where
        isCentral = idIsPrime && noZeros && lPartsArePrime && rPartsArePrime
        isLeft = idIsPrime && noZeros && lPartsArePrime
        isRight = idIsPrime && noZeros && rPartsArePrime
        idIsPrime = isPrime id
        noZeros = not $ elem 0 digits
        lPartsArePrime = checkIfPrimes $ lParts digits
        rPartsArePrime = checkIfPrimes $ rParts digits
        digits = numToDigits id
        xParts _ [] = []
        xParts f digits = map digitsToNum (filter (not . null) $ f digits)
        lParts = xParts tails
        rParts = xParts inits

main = do
    t <- fmap read getLine :: IO Int

    forM_ [1..t] $ \x -> do
        id <- fmap read getLine :: IO Integer

        putStrLn $ captainPrime id
        
    return ()

