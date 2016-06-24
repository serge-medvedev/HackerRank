-- https://www.hackerrank.com/challenges/fp-list-length

len :: [a] -> Int
len lst = sum [1 | _ <- lst]

