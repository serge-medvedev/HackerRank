-- https://www.hackerrank.com/challenges/sequence-full-of-colors

import qualified Data.Array as A

checkColors colors = checkStats 0 0 0 0 1
    where
        checkStats nR nG nB nY position
            | position > (snd $ A.bounds colors) = (nR == nG) && (nY == nB)
            | (abs $ nR-nG) > 1 = False
            | (abs $ nB-nY) > 1 = False
            | otherwise =
                checkStats
                    (if (colors A.! position) == 'R' then (nR+1) else nR)
                    (if (colors A.! position) == 'G' then (nG+1) else nG)
                    (if (colors A.! position) == 'B' then (nB+1) else nB)
                    (if (colors A.! position) == 'Y' then (nY+1) else nY)
                    (position+1)

process_tcases 0 = return ()
process_tcases t = do
    input <- getLine
    
    let colors = A.listArray (1,length input) input
    
    putStrLn $ show $ checkColors colors
    
    process_tcases (t-1)

main = do
    input <- getLine
    
    let t = (read :: String -> Int) input
    
    process_tcases t

