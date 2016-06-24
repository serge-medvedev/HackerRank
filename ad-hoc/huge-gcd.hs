-- https://www.hackerrank.com/challenges/huge-gcd-fp

gcd' :: Integral a => a -> a -> a
gcd' x y
    | y == 0 = x
    | otherwise = gcd' y (mod x y)

hugeGCD :: [Integer] -> [Integer] -> Integer
hugeGCD ns ms = (gcd' (product' ns) (product' ms)) `mod` 1000000007
    where product' xs = foldl (\x acc -> (acc * x)) 1 xs

main = do
    input <- getLine
    let n = (read :: String -> Integer) input
    input <- getLine
    let ns = map (read :: String -> Integer) (words input)
    input <- getLine
    let m = (read :: String -> Integer) input
    input <- getLine
    let ms = map (read :: String -> Integer) (words input)
    
    putStr $ show $ hugeGCD ns ms

