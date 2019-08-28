f :: Integer -> Integer -> Integer
f x y = 2*x + y

addTup :: (Integer, Integer) -> Integer
addTup tup = (fst tup) + (snd tup)

addTrip :: (Integer, Integer, Integer) -> Integer
addTrip (x,y,z) =  x + y + z

keys assocs = [x | (x,y) <- assocs]

isEmpty (x:xs) = False
isEmpty [] = True

evens = [2,4..]

isLucky 3 = True
isLucky 7 = True
isLucky x = False

cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea =pi * r^2
    in sideArea + 2*topArea

cylinder2 r h = sideArea + 2*topArea
    where sideArea = 2 * pi * r * h
          topArea =pi * r^2

cylinder3 r h = rectangle h (2*pi*r) + 2*(circle r)
    where rectangle l w = l * w
          circle rad = pi*rad^2
