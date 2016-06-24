-- https://www.hackerrank.com/challenges/fp-filter-positions-in-a-list

f :: [Int] -> [Int]
f lst = [a | (a,i) <- zip lst [1..], even i]

main = do
   inputdata <- getContents
   mapM_ (putStrLn. show). f. map read. lines $ inputdata

