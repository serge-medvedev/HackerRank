-- https://www.hackerrank.com/challenges/fp-list-replication

f :: Int -> [Int] -> [Int]
f n arr = concat $ map (\a -> replicate n a) arr

main :: IO ()
main = getContents >>=
       mapM_ print. (\(n:arr) -> f n arr). map read. words

