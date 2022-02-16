compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred x = compare 100 x  
-- partial function solution to problem above
-- compare 100 returns a function that takes a number and compares it with 100
compareWithHundred' :: (Num a, Ord a) => a -> Ordering  
compareWithHundred' = compare 100 

-- use sections to partially apply infix functions
divideByTen :: (Floating a) => a -> a
divideByTen  = (/10)

-- check if a character supplied to it is an uppercase letter as a partial function
isUpperAlphanum :: Char -> Bool  
isUpperAlphanum = (`elem` ['A'..'Z']) 

-- ONLY EXCEPTION: The negative (-) sign; do not use something like (-4)
subFour :: Integer -> Integer 
subFour = (subtract 4)  

-- use running this example which return 2:   app (\x -> x+1) 1
-- example takes a function (which takes x and returns x+1) 
--               and x (which is used in the supplied function)
app :: (a -> b) -> a -> b
app f x = f x

-- Functions can take functions as parameters and also return functions
applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

