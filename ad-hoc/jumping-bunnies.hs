-- https://www.hackerrank.com/challenges/jumping-bunnies

import Data.List
import qualified Data.Map as M

primeFactorization 0 = [0]
primeFactorization 1 = [1]
primeFactorization x = primeFactorization' x [2..x]
    where
        primeFactorization' x [] = []
        primeFactorization' x (d:ds)
            | x `mod` d == 0 = d : (primeFactorization' (x `div` d) [d..x])
            | otherwise = primeFactorization' x ds

jumpingBunnies n js = product $ concat [replicate n x | (x,n) <- M.toList fsCombination]
    where
        primeFs = [M.fromListWith (+) [(item, 1) | item <- f] | f <- [primeFactorization j | j <- js]]
        fsCombination = M.unionsWith max primeFs

main = do
    n <- fmap read getLine :: IO Int
    js <- fmap (map read . words) getLine :: IO [Int]

    putStrLn $ show $ jumpingBunnies n js

