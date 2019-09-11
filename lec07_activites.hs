-- return second to last element
secondToLast :: [a] -> a
secondToLast [] = error "AAAAH"
secondToLast [x] = error "AAAH!"
secondToLast [x,y] = x
secondToLast (x:xs) = secondToLast xs

--return all the elements with duplicates removed. Assume the list is sorted.
removeDuplicates :: [Integer] -> [Integer]
removeDuplicates [] = []
removeDuplicates [x] = [x]
removeDuplicates (x:y:xs) = 
    let noDupsXs = removeDuplicates (y:xs)
    in if x == y
       then noDupsXs
       else x:noDupsXs


--removeDuplicates [1,7,7,10,20,20] = [1,7,10,20]

--If you complete that, try to do remDups without assumping it's sorted.
remDups :: Eq a => [a] -> [a]
remDups [] = []
remDups (x:xs) = 
    let noDupsXs = remDups xs
    in if x `elem` noDupsXs
       then noDupsXs
       else x:noDupsXs

