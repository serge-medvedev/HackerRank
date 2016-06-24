-- https://www.hackerrank.com/challenges/string-reductions

import Data.List

remove_duplicates [] = []
remove_duplicates str = fst $ mapAccumL (\acc x -> (if elem x acc then acc else (acc ++ [x]),x)) [] str

main = do
    input <- getLine
    putStr $ remove_duplicates input

