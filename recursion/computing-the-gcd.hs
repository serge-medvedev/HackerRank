-- https://www.hackerrank.com/challenges/functional-programming-warmups-in-recursion---gcd

module Main where

gcd' :: Integral a => a -> a -> a
gcd' x y
    | y == 0 = x
    | otherwise = gcd' y (mod x y)

main = do
  input <- getLine
  print . uncurry gcd' . listToTuple . convertToInt . words $ input
 where
  listToTuple (x:xs:_) = (x,xs)
  convertToInt = map (read :: String -> Int)

