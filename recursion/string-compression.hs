-- https://www.hackerrank.com/challenges/string-compression

import Data.List
import Control.Monad

compressString str = [(g !! 0,length g) | g <- group str]

main = do
    input <- getLine
    
    let compressedString = compressString input
    
    forM_ compressedString $ \(ch,n) -> do
        putStr $ [ch] ++ (if n == 1 then "" else show n)

