import Data.List (union)

-- My solution
first1 :: Integer -> Integer
first1 x = sum $ filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [3..x-1]

-- Inspired by others
other1 :: Integer -> Integer
other1 x = sum [n | n <- [3..x-1], n `mod` 5 == 0 || n `mod` 3 == 0]

other2 :: Integer -> Integer
other2 x = sum [3,6..x-1] + sum [5,10..x-1] - sum [15,30..x-1]

-- My inspired solution
euler1 :: Integer -> Integer
euler1 x = sum $ [3,6..x-1] `union` [5,10..x-1]
