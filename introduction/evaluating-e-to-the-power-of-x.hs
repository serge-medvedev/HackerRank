-- https://www.hackerrank.com/challenges/eval-ex

solve' :: Double -> Int -> Double
solve' x 0 = 1.0
solve' x y = ((x^y)/(fromIntegral $ product [1..y])) + solve' x (y-1)

solve :: Double -> Double
solve x = solve' x 9

main :: IO ()
main = getContents >>= mapM_ print. map solve. map (read::String->Double). tail. words

