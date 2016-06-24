-- https://www.hackerrank.com/challenges/prefix-compression

prefix_compression x y = (p,x',y')
    where
        y' = drop pLength y
        x' = drop pLength x
        p = take pLength x
        pLength = calcPrefixLength (zip x y) 0
        calcPrefixLength [] acc = acc
        calcPrefixLength ((a,b):rest) acc | a == b = calcPrefixLength rest (acc+1) | otherwise = acc

main = do
    x <- getLine
    y <- getLine
    
    let (p,x',y') = prefix_compression x y
    
    putStrLn $ (show $ length p) ++ " " ++ p
    putStrLn $ (show $ length x') ++ " " ++ x'
    putStrLn $ (show $ length y') ++ " " ++ y'

