compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred x = compare 100 x  
-- partial function solution to problem above
-- compare 100 returns a function that takes a number and compares it with 100
compareWithHundred' :: (Num a, Ord a) => a -> Ordering  
compareWithHundred' = compare 100 

-- check if a character supplied to it is an uppercase letter as a partial function
isUpperAlphanum :: Char -> Bool  
isUpperAlphanum = (`elem` ['A'..'Z']) 

-- ONLY EXCEPTION: Instead of doing (-4)
subFour :: Integer -> Integer 
subFour = (subtract 4)  

app :: (a -> b) -> a -> b -- use running this example which return 2:   app (\x -> x+1) 1
app f x = f x
