--assignment 1
and1 :: [Bool] -> Bool
and1 bs =  not( null bs) && head bs == True && and1 (tail bs)

or1 :: [Bool] -> Bool
or1 bs =    not(null bs) && head bs == True || or1 (tail bs)

issorted :: [Int] -> Bool
issorted [] = True
issorted ( n : []) = True
issorted (n:ns) = n < (head ns) && issorted ns  

range1 :: Int -> Int -> [Int]
range1 lo hi    | lo == hi = hi :[]
               | lo > hi = [] 
               | otherwise = lo : range1 (lo +1 ) hi

copies :: Int -> a -> [a]               
copies 0 x = []
copies n x = x : copies (n-1) x

--assignment 2
applyAll :: [(Int -> Int)] -> Int -> [Int]
applyAll [] x = []
applyAll (f:fs) x = f x : applyAll fs x

remove :: ( Int -> Bool) -> [Int] -> [Int]
remove p [] = []
remove p (x:xs) = if p x then 
                     remove p xs
                  else
                     x :remove p xs
--OR

remove1 p = foldr ( \n acc -> if p n then acc else n : acc) [] 

count:: Eq a => a -> [a] -> Int
count x [] = 0
count x (n:ns) = if x == n then
                  1 + count x ns
                 else
                 count x ns
--OR
count1 x = foldr ( \n acc -> if x == n then acc + 1 else acc) 0 


--maximum n:[] = n
--maximum n:ns = filter( \x -> x > n) ns
maximums :: [Int] -> Int
maximums ns = maximums' ns 0

maximums' :: [Int] -> Int -> Int
maximums' [] z = z
maximums' (n:ns) z = if n > z then
                  maximums' ns n
                  else
                  maximums' ns z
                  
--OR

maximum2 ns = foldr(  \n acc -> if n > acc then n else acc) (head ns) ns

append :: [Int] -> [Int] -> [Int]
append xs ys = foldr( \x acc -> x : acc ) ys xs

--assignment 3
partialSums :: [Int] -> [Int]
partialSums [] = []
partialSums ns =   partialSums' ns 0

partialSums' :: [Int] -> Int -> [Int]
partialSums' [] _ = []
partialSums' (n:ns) acc = n + acc : partialSums' ns (n + acc) 

powers :: Int -> [Int]
powers n = n : powers' n n

powers' :: Int -> Int -> [Int]
powers' n acc = n*acc : powers' n (n*acc)

--OR
powers1 :: Int -> [Int]
powers1 n = n : map( \x -> x * n) (powers1 n)

factorial :: Int -> Int
factorial 0 = 1
factorial n = factorial(n - 1) * n

factorials :: [Int]
factorials = [factorial n | n <- [1..]]

--OR
factorials1 :: [Int]
factorials1 = 1 : zipWith( \n m -> n*m) factorials1 [2..]

--assignment 4
approx :: Float -> [Float]
approx x = 1.0 : map( \n -> (n + x/n)/2) (approx x) 

squareRoot :: Float -> Float
squareRoot x = squareRoot' (head (approx x)) (tail (approx x))

squareRoot' :: Float -> [Float] -> Float        
squareRoot' y (z:zs) = if (abs(z-y)) < 0.0001 then
                    z 
                  else 
                  squareRoot' z zs

primes :: [Int]
primes = 2: primes' [3,5..]

primes' :: [Int] -> [Int]
primes' (n:ns) = if indivisible n == [] then 
                  n : primes' ns 
                 else 
                  primes' ns
indivisible :: Int -> [Int]
indivisible n = [d | d <- (takeWhile(\x -> x <= 
           floor( squareRoot(fromIntegral n) )) primes), mod n d == 0]  
           
--OR

primes1 :: [Int]  
primes1 = 2: [p|p <- [3,5..], isPrime p ]

--isprime n : Checks if n has zero factors
isPrime :: Int -> Bool
isPrime n = factors n == []

--factors n : Checks if values from primes less than squareRoot n are factors
-- of n
factors :: Int -> [Int]
factors n = [f|f<- (takeWhile(\x -> x <= 
           floor( squareRoot(fromIntegral n) )) primes), mod n f == 0]    

--assignment 5
integers :: [Int]
integers = 0 : integers' [1..]

integers' :: [Int] -> [Int]
integers' (n:ns) = n: -n : integers' ns 

runs :: Eq a => [a] -> Int
runs [] = 0

runs ( x: xs) = runs' x xs 1


runs' :: Eq b => b ->  [b] -> Int -> Int
runs' acc (x:[]) result =
        if acc == x then
                result
        else
                (result + 1)

runs' acc ( x : xs ) result =
        if acc == x then
              runs' acc xs result
        else
              runs' x xs (result + 1)     
              
occurences :: Eq a => [a] -> [(a, Int)]
occurences [] = []
occurences (x:xs) = (x,occurs x (x:xs)) : occurences (delete x xs)

occurs :: Eq a => a -> [a] -> Int
occurs x xs = length (filter(\f -> f == x)  xs)

delete :: Eq a => a -> [a] -> [a]
delete x xs = filter(\f -> f /= x ) xs
