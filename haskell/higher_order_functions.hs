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

-- takes a function and returns a function where the first two arguments are flipped
flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

-- quicksort using filter function
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
     let smallerSorted = quicksort (filter (<=x) xs)
         biggerSorted = quicksort (filter (>x) xs)
     in smallerSorted ++ [x] ++ biggerSorted

--  find the largest number under 100,000 that's divisible by 3829
largestDivisible :: (Integral a) => a  
largestDivisible = head (filter p [100000,99999..])  
    where p x = x `mod` 3829 == 0  

-- Collatz sequence problem:
--     for all starting numbers between 1 and 100, how many chains have a length greater than 15?
chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)  

numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15

-- Lambda example of numLongChains
numLongChains' :: Int  
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100])) 

-- sum implementation using a fold instead of explicit recursion
sum' :: (Num a) => [a] -> a  
sum' xs = foldl (\acc x -> acc + x) 0 xs 

-- sum implementantion using folds with currying
sum'' :: (Num a) => [a] -> a  
sum'' = foldl (+) 0 

-- map function implemented using the right fold
map' :: (a -> b) -> [a] -> [b]  
map' f xs = foldr (\x acc -> f x : acc) [] xs 

-- standard library function using foldr1
maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc) 

-- function below traverses all the way from the right until the last element 
head' :: [a] -> a  
head' = foldr1 (\x _ -> x)
 


