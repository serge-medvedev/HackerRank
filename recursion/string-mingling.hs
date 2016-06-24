-- https://www.hackerrank.com/challenges/string-mingling

module Main where

mingle_strings p q = concat [[a,b] | (a,b) <- zip p q]

main = do
    p <- getLine
    q <- getLine
    putStr $ mingle_strings p q

