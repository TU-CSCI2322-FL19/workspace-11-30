
--prodEvensLc :: [a] -> a
prodEvensLc lst = product [ x | x <- lst, even x]

-- Write a recursive function prodEvensRec that takes a list and returns the product of all even
-- numbers.
--prodEvensRec lst 
--prodEvensRec [x] = if even x then x * x else 0
prodEvensRec [] = 0
prodEvensRec(x:xs) = (if even x then x else 0) * prodEvensRec x



-- Write a recursive function myMaximum that takes a list and returns the largest element.


