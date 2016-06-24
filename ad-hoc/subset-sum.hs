-- https://www.hackerrank.com/challenges/subset-sum

import Data.List
import Data.Maybe
import Control.Monad
import qualified Data.Set as S

main = do
    n <- fmap read getLine :: IO Integer
    anArray <- fmap (map read . words) getLine :: IO [Integer]
    t <- fmap read getLine :: IO Int
    sVariants <- fmap (map read . lines) getContents :: IO [Integer]

    let anArray' = sortBy (\lhs rhs -> compare rhs lhs) anArray
    let sumProgression = S.fromAscList $ scanl1 (+) anArray'

    forM_ sVariants $ \s -> do
        let result = case S.lookupGE s sumProgression of
                Just a -> (S.findIndex a sumProgression) + 1
                Nothing -> -1

        putStrLn $ show result

