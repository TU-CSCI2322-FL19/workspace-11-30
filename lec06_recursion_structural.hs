union :: [a] -> [a] -> [a]
union xs ys = xs ++ ys

intersection :: Eq a => [a] -> [a] -> [a]
intersection xs ys = [x | x <- xs, x `elem` ys]

setDifference :: Eq a => [a] -> [a] -> [a]
setDifference xs ys = [x | x <- xs, not (x `elem` ys)]

crossProduct :: [a] -> [b] -> [(a,b)]
crossProduct xs ys = [ (x,y) | x <- xs, y <- ys]

subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = null [x | x <- xs, not (x `elem` ys)]
            --  [] == [x | x <- xs, not (x `elem` ys)]


powerset :: Ord a => [a] -> [[a]]
powerset xs = [[]] ++ [[x] | x <- xs] ++ [ [x,y]  | x <- xs, y <- xs, x < y]
              ++ [ [x,y,z] | x <- xs, y <- xs, z <- xs, x < y, y < z]

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

myLength :: [a] -> Integer
myLength [] = 0 
myLength (x:xs) = 1 + myLength xs

mySum :: [Integer] -> Integer
mySum [] = 0
mySum (x:xs) = 
  let sumOfXs = mySum xs
  in x + sumOfXs
               
