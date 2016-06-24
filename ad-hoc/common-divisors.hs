-- https://www.hackerrank.com/challenges/common-divisors

import qualified Data.Set as S

findCommonDivisorsNumber m l = S.size $ S.intersection mDivisors lDivisors
    where
        mDivisors = S.fromList $ concat [[mI,m `div` mI] | mI <- [1..(ceiling $ sqrt $ fromIntegral m)], (m `mod` mI) == 0]
        lDivisors = S.fromList $ concat [[lI,l `div` lI] | lI <- [1..(ceiling $ sqrt $ fromIntegral l)], (l `mod` lI) == 0]

processTCases 0 = return ()
processTCases t = do
    input <- getLine
    
    let (m:l:_) = [(read :: String -> Int) w | w <- words input]
    
    putStrLn $ show $ findCommonDivisorsNumber m l
    
    processTCases (t-1)
    
main = do
    input <- getLine
    
    let t = (read :: String -> Int) input
    
    processTCases t

