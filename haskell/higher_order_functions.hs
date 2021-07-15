compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred x = compare 100 x  
-- partial function solution to problem above
-- compare 100 returns a function that takes a number and compares it with 100
compareWithHundred' :: (Num a, Ord a) => a -> Ordering  
compareWithHundred' = compare 100 

-- check if a character supplied to it is an uppercase letter as a partial function
isUpperAlphanum :: Char -> Bool  
isUpperAlphanum = (`elem` ['A'..'Z']) 

-- Instead of doing (-4)
subFour :: Integer -> Integer 
subFour = (+4)