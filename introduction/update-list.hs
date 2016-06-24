-- https://www.hackerrank.com/challenges/fp-update-list

f arr = map abs arr

main = do
   inputdata <- getContents
   mapM_ putStrLn $ map show $ f $ map (read :: String -> Int) $ lines inputdata

