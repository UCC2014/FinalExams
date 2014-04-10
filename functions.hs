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



------------------------------------------------
-- Haitao Yin 
------------------------------------------------
-- Assignment 1 
-- 1. and bs                                  --
-- Do "and" operation to each item in a list  --
------------------------------------------------


and = \bs -> 
-- if bs == null then True
    if null bs then
        True
    else
-- Recursive invoke function and, make the list 
-- like this: a1 && a2 && a3 && ... && an && True
        head bs && and ( tail bs )

------------------------------------------------
-- 2. or bs                                   --
-- Do "or" operation to each item in a list   --
------------------------------------------------

or = \bs -> 
-- if bs == null then False
    if null bs then
        False
    else
-- Recursive invoke function or, make the list 
-- like this: a1 || a2 || a3 || ... || an || False
        head bs || or ( tail bs )


------------------------------------------------
-- 3. issorted ns                             --
-- Sorted the list in ascending order.        --
------------------------------------------------

issorted = \ ns -> 
    if length ns <= 1 then 
-- if list only have 1 or 0 items ,it's true.
        True 
-- compare with the first and second item.
--          else if head ns >= head (tail(ns)) then 
--              False
    else 
-- Recursive invoke function issorted && compare with the first and second item.
        head ns <= head (tail(ns)) && issorted (tail(ns))

------------------------------------------------
-- 4. range lo hi                             --
-- Create a list from first argument to       --
-- second argument by ascending, step 1       --
------------------------------------------------

range = \ lo -> \ hi -> 
-- if lo bigger or equals hi then return empty
    if lo >= hi then
        []
    else
-- Recursive invoke function range
        lo:(range (lo + 1) hi)

------------------------------------------------
-- 5. copies n x                              --
-- Create a list have n items,each item is x  --
------------------------------------------------

copies = \ n -> \ x ->
    if n <= 0 then
        []
    else 
-- Recursive invoke function copies 
        x:copies (n-1) x


--  Assignment 2 

-- 1.applyAll fs x 
applyAll = \fs -> \x -> foldr ( \f -> \acc -> f x:acc ) [] fs

-- 2.remove p xs 
remove = \p -> foldr ( \x -> \acc -> if p x then acc else x:acc ) []

-- 3. count x xs
count = \p -> foldr ( \x -> \acc -> if p == x then acc + 1 else acc ) 0

-- 4. maximum ns
maximum = foldl1 ( \x -> \y -> if x > y then x else y )   

-- 5. append xs ys
append = \xs -> \ys -> foldr ( \x -> \y -> x : y ) ys xs


--  Assignment 3

-- 1. partialSums ns
partialSums = partialSums' 0
partialSums' = \sumSoFar -> \ns -> 
    if null ns then
       []    
    else                                                  
        sumSoFar + head ns : partialSums' (sumSoFar + head ns) (tail ns)

-- 2. powers n
powers = \n -> powers' 1 n
powers' = \x -> \n ->  (x*n) : powers' (x*n) n 

-- 3. factorials
factorials = 1 : factorials' 1 2
factorials' = \x -> \n -> (x*n) : factorials' (x*n) (n+1)

--  Assignment 4

-- 1. approx x
-- take 4 ( approx 2.0 ) => [1.0,1.5,1.4166667,1.4142157]

--a :: Int -> Float -> Float
--a 1 x = 1.0
--a i x = (a (i-1) x + x / a (i-1) x) / 2.0
--approx :: Float -> [Float]
--approx x = [a n x | n <- [1..]]

approx :: Float -> [Float]
approx x = approx' 1.0 x
approx' :: Float -> Float -> [Float]
approx' n x = n : approx' ((n + x / n) / 2.0 ) x

-- 2. squareRoot x
-- squareRoot 2.0 => 1.4142135 

squareRoot :: Float -> Float
squareRoot x = squareRoot' (approx x)
squareRoot' :: (Fractional a, Ord a) => [a] -> a
squareRoot' (x:y:xs) 
    | abs (x - y) < 0.0001  = y
    | abs (x - y) >= 0.0001 = squareRoot' (y:xs)

-- 3. primes

--primes = 2:primes' [3,5..]
--primes' (x:xs) = x : primes' (filter (\y ->y `mod` x /= 0) xs) 
--primes' (x:xs) = x : primes' [ y | y<-xs, mod y x /= 0 ]

-- primes' ps xs ns
-- "ps" is the Prime number list,
-- "xs" store the cache Prime number list as the divisor,
-- "ns" is the dividend which store a infinite odd number list.
primes :: [Integer]
primes = 2 : primes' [2] [2] [3,5..]
primes' :: Integral a => [a] -> [a] -> [a] -> [a]
primes' ps (x:xs) (n:m:ns)
    | n < x * x    = n : (primes' (ps ++ [n]) (ps ++ [n]) (m:ns)) 
    | mod n x == 0 = primes' ps ps (m:ns) 
    | mod n x /= 0 = primes' ps xs (n:m:ns)

--  Assignment 5

-- 1. integers
integers :: [Integer]
integers = [ (div x 2) * ((-1) ^ x) | x <- [1..] ]


-- 2. runs xs
runs :: (Eq a1, Num a) => [a1] -> a
runs [] = 0
runs [ _ ] = 1
runs (x:y:xs) 
    | x == y = runs (y:xs) + 0
    | x /= y = runs (y:xs) + 1


-- 3. occurrences x
-- occurrences "mississippi" => [('m', 1), ('i', 4), ('s', 4), ('p', 2)]
occurrences :: [Char] -> [(Char, Int)]
occurrences [] = [] 
occurrences x = [(a, count a x) | a <- (eachelem x)]
-- count x xs from a2.hs
count :: Eq a => a -> [a] -> Int
count = \p -> foldr ( \x -> \acc -> if p == x then acc + 1 else acc ) 0
-- eachelem "mississippi" => "misp"
eachelem :: [Char] -> [Char]
eachelem [] = []
eachelem (x:xs) = x : eachelem [ y | y <- xs, y /= x ]

