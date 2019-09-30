import Debug.Trace
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

positives :: [Int] -> [Int]
positives [] = []
positives (x:xs) =
    if x > 0
    then x:(positives xs)
    else positives xs

upperCase :: String -> String
upperCase "" = ""
upperCase (c:cs) =
    if c `elem` ['A'..'Z']
    then c:(upperCase cs)
    else upperCase cs

--onlyThe :: [Int] -> [Int]
onlyThe p [] = []
onlyThe p (x:xs) =
    if p x 
    then x:(onlyThe p xs)
    else onlyThe p xs

evens [] = []
evens (x:xs) =
    if x `mod` 2 == 0
    then x:(evens xs)
    else evens xs

singletons lst = map aux lst
    where aux x = [x]

evens2 lst = onlyThe aux lst
    where aux x = x `mod` 2 == 0

singletons2 lst = map (\x -> [x]) lst
evens3 lst = onlyThe (\x -> (x `mod`2) ==0) lst


multPairs :: [Int] -> [Int] -> [Int]
multPairs (x:xs) (y:ys) = (x*y):(multPairs xs ys)
multPairs [] [] = []

concatPairs :: [String] -> [String] -> [String]
concatPairs (x:xs) (y:ys) = (x++y):(concatPairs xs ys)
concatPairs [] [] = []

funcPairs f [] [] = []
funcPairs f xs [] = []
funcPairs f [] ys = []
funcPairs f (x:xs) (y:ys) = (f x y):(funcPairs f xs ys)

katamari :: (a -> b -> b) -> b -> [a] -> b
katamari f b [] = b
katamari f b (s:ss) = f s (katamari f b ss)

multPositives :: [Int] -> Int 
--with a list comprehension and built-in functions
--with recursion
--with a filter and fold
--just with a fold

myElem :: Eq a => a -> [a] -> Bool
--with a filter and built-in functions
--with recursion
--with a fold

