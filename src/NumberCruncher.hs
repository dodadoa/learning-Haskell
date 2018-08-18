module NumberCruncher
( countByCondition
, countEven
, countPositive
, countNegative
) where

countByCondition ::  (Int -> Bool) -> [Int] -> Int
countByCondition p = length . filter p

countEven :: [Int] -> Int
countEven = countByCondition (\a -> a `mod` 2 == 1)

countOdd :: [Int] -> Int
countOdd = countByCondition (\a -> a `mod` 2 == 0)

countPositive :: [Int] -> Int
countPositive = countByCondition (>= 0)

countNegative :: [Int] -> Int
countNegative = countByCondition (< 0)
