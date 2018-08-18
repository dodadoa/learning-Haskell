module Shape
( area
, perimeter
, maxArea
, maxPerimeter
, max
, Shape (Circle, Rectangle, Square)
) where

data Shape = Circle Float | Rectangle Float Float | Square Float deriving(Show)
type Area = Float
type Perimeter = Float

area :: Shape -> Area
area (Circle r) = pi * r * r
area (Rectangle w l) = w * l 
area (Square s) = s * s

perimeter :: Shape -> Perimeter
perimeter (Circle r) = 2 * pi * r
perimeter (Rectangle w l) = (2 * w) + (2 * l)
perimeter (Square s) = s * 4

maxArea :: [Shape] -> Area
maxArea l = maximum $ map area l

maxPerimeter :: [Shape] -> Perimeter
maxPerimeter l = maximum $ map perimeter l