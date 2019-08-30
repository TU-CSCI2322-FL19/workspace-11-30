lst = [1..10000]
x = 7
i = sum lst
j = sum [sum (x:lst) | x <- lst]
k = sum [sum (lst++[x]) | x <- lst]

myHead :: [a] -> a
myHead (x:xs) = x
myHead [] = error "empty list passed to myHead" 

