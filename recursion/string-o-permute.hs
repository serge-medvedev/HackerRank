-- https://www.hackerrank.com/challenges/string-o-permute

permute str
    | null str = return ()
    | otherwise = do
        mapM_ (\(a,b) -> putStr [b,a]) ( groupNeighbours str )
        putStrLn ""
    where
        groupNeighbours [] = []
        groupNeighbours (x:y:rest) = (x,y):(groupNeighbours rest)

process_tcases 0 = return ()
process_tcases n = do
    str <- getLine
    permute str
    process_tcases (n-1)

main = do
    input <- getLine
    let t = (read :: String -> Int) input
    process_tcases t

