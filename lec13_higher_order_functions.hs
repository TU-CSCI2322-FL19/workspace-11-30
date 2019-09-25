add4 x = x + 4
add8 x = x + 8

addY y x = x + y

add8All [] = []
add8All (x:xs) = (x+8):(add8All xs)

add16All y [] = []
add16All y (x:xs) = (x+y):(add16All y xs)

addYAll y [] = []
addYAll y (x:xs) = (x+y):(addYAll y xs)

absAll [] = []
absAll (x:xs) = (abs x):(absAll xs)

doubleAll [] = []
doubleAll (x:xs) = (2*x):(doubleAll xs)

doToAll :: (a -> b) -> [a] -> [b]
doToAll f [] = []
doToAll f (x:xs) = (f x):(doToAll f xs)

dAdd :: Int -> Int -> Int
dAdd x y = x+2*y
