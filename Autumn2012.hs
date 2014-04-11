--Autumn 2012

--Q1

--(a)
drop1 :: Int -> [a] -> [a]
drop1 n xs | n == 0 = xs
          | n < 0 = xs
          | n > length xs = []
          | otherwise = remove n xs 0
          
remove :: Int -> [a] -> Int -> [a]         
remove n xs acc = if n == acc then
                   xs
                   else
                   remove n (tail xs) (acc +1)

--(b)
dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 p [] = []                   
dropWhile1 p (x:xs) = if p x then 
                      dropWhile1 p xs
                      else
                      x : dropWhile1 p xs

--(c)
fibonacci :: [Int]                      
fibonacci  = 0 : 1 : zipWith( + ) fibonacci (tail fibonacci)

--Q2 ALREADY DONE IN SUMMER 2013 PAPER

--Q3A
zipWith1 :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith1 f [] ys = []
zipWith1 f xs [] = []
zipWith1 f (x:xs) (y:ys) = f x y : zipWith1 f xs ys  

--Q3B
-- 4,5,8,13,15
-- More meaningful name would be partialSums
--its type would be 
--partialSums :: [Int] -> [Int]

--Q3C 

partialSums :: [Int] -> [Int]
partialSums [] = []
partialSums ns =  partialSums' ns 0
partialSums' :: [Int] -> Int -> [Int]
partialSums' [] acc = []
partialSums' (n:ns) acc = (n + acc) : partialSums' ns (n+acc)   
