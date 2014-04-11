--Q1
--(a)
allEqual :: Eq a => [a] -> Bool
allEqual (x:y:xs) = x == y && allEqual (y:xs)
allEqual _ = True

--(b)
allDifferent :: Eq a => [a] -> Bool
allDifferent xs = allDifferent' [] xs

-- allDifferent' seen xs : is every item in the list 'xs' not in the list 'seen'?
allDifferent' :: Eq a => [a] -> [a] -> Bool
allDifferent' _ [] = True
allDifferent' seen (x:xs) = notElem x seen && allDifferent' (x:seen) xs

--(c)
countMax :: Ord a => [a] -> Int
countMax (x:xs) = countMax' x 1 xs

-- countMax' maxSoFar count xs :
countMax' :: Ord a => a -> Int -> [a] -> Int
countMax' maxSoFar count (x:xs)
    | x < maxSoFar = countMax' maxSoFar count xs
    | x > maxSoFar = countMax' x 1 xs
    | otherwise = countMax' maxSoFar (count+1) xs
countMax' _ count [] = count

--Q2
--(a)
-- I am using id2 in place of id, to avoid conflicts
id2 :: a -> a
id2 x = x

--(b)

-- I am using (#.) in place of (.), to avoid conflicts
(#.) :: (a -> b) -> (c -> a) -> c -> b
(f #. g) x = f (g x)

--(c), (d)
compose :: [(a -> a)] -> a -> a
compose (f:fs) x = f (compose fs x)
compose [] x     = x

--(e)
compose2 :: [(a -> a)] -> a -> a
compose2 fs x = foldr (\f -> \acc -> f acc) x fs

--Q3
--(a)
zipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith2 f (x:xs) (y:ys) = (f x y):zipWith f xs ys
zipWith2 _ [] _          = []
zipWith2 _ _ []          = []

--(b)
factorials :: [Integer]
factorials = 1:zipWith (*) factorials [1 ..]


--(c)
-- I found this here: http://igortonky.blogspot.ie/2008/06/haskell-power.html
pyTriple :: [(Integer,Integer,Integer)]
pyTriple = [(x,y,z) | z <- [1..], y <- [1..z-1], x <- [1..y-1], x*x + y*y == z*z ]
