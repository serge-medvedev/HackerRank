-- https://www.hackerrank.com/challenges/area-under-curves-and-volume-of-revolving-a-curv

import Text.Printf (printf)

f :: Double -> [Double] -> [Double] -> Double
f x a b = sum $ zipWith (\a' b' -> a'*(x**b')) a b

calc_area :: Double -> Double -> [Double] -> [Double] -> Double -> Double
calc_area l r a b acc
    | l > r = acc
    | otherwise =
        let acc' = acc + (0.001 * (f l a b))
            l' = l + 0.001
        in calc_area l' r a b acc'

calc_volume :: Double -> Double -> [Double] -> [Double] -> Double -> Double
calc_volume l r a b acc
    | l > r = acc
    | otherwise =
        let acc' = acc + (pi * ((f l a b)**2) * 0.001)
            l' = l + 0.001
        in calc_volume l' r a b acc'

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b =
    let area = calc_area (fromIntegral l) (fromIntegral r) (map fromIntegral a) (map fromIntegral b) 0.0
        volume = calc_volume (fromIntegral l) (fromIntegral r) (map fromIntegral a) (map fromIntegral b) 0.0
    in [area, volume]

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines

