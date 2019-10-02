prodList lst = foldr (\x acc -> x * acc) 1 lst
prodList2 lst = foldr (*) 1 lst --an excellent simplification
prodList3 :: [Int] -> Int
prodList3 = foldr (*) 1 --a simplification too far

myMaximum [] = error ":("
myMaximum (x:xs) = foldr max x xs
myMaximum2 lst = foldr1 max lst
