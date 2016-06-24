-- https://www.hackerrank.com/challenges/rotate-string

rotate_string str = rotate_string' str 1
    where
        n = length str
        rotate_string' (x:xs) i
            | i > n = []
            | otherwise = (xs ++ [x]):(rotate_string' (xs ++ [x]) (i+1))

process_tcases 0 = return ()
process_tcases t = do
    input <- getLine
    mapM_ (\str -> putStr $ str ++ " ") (rotate_string input)
    putStrLn ""
    process_tcases (t-1)

main = do
    input <- getLine
    let t = (read :: String -> Int) input
    process_tcases t

