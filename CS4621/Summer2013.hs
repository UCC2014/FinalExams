--Q1

--(a)
data List a = EmptyList | L a (List a) deriving Show

fromList :: [a] -> List a
fromList []     = EmptyList
fromList (x:xs) = L x (fromList xs) 

(#:) :: a -> List a -> List a
(#:) x xs = L x xs

--(b)
count :: (Eq a) => a -> List a -> Int
count _ EmptyList = 0
count x (L y xs)
	| y == x    = 1 + count x xs
	| otherwise = count x xs

append :: List a -> List a -> List a
append EmptyList ys = ys
append (L x xs) ys  = L x (append xs ys)

--Q2

--Q3

--(a)
