doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = if x > 100 then x else x*2
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
-- Problem: which right triangle that has integers for all sides and all sides equal to 
--          or smaller than 10 has a perimeter of 24.
rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]
-- declare explict type declaration
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z
factorial :: Integer -> Integer
factorial n = product [1..n]
circumference :: Float -> Float
circumference r = 2 * pi * r
-- Double has double the precision
circumference' :: Double -> Double  
circumference' r = 2 * pi * r