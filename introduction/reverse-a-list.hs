-- https://www.hackerrank.com/challenges/fp-reverse-a-list

rev [] = []
rev (x:xs) = (rev xs) ++ [x]

