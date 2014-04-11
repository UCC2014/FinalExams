--Q1

--(a)
data List a = EmptyList | L a (List a) deriving Show

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

--Not done yet

--Q3

--(a)

-- z takes an integer s1 (in this case it is 3) and sums the top s1 elements on
-- the stack, the result will be on top of the stack.
-- After the example is run, the stack is as follows:
--                                                    18
--                                                    4
--                                                    3
--                                                    2
--                                                    1

--(b) Not done yet