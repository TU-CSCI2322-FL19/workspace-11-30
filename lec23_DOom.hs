safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead [] = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (x:xs) = Just xs
safeTail [] = Nothing

safeDivide :: Double -> Double -> Maybe Double
safeDivide num 0 = Nothing
safeDivide num denom = Just (num / denom)

lst = [5,7]

firstOverSecondDanger = (head lst) / (head $ tail lst)

firstOverSecond :: Maybe Double
firstOverSecond = 
  case safeHead lst of
      Nothing -> Nothing
      Just elem -> case safeTail lst of
                      Nothing -> Nothing
                      Just tl -> case safeHead tl of
                                    Nothing -> Nothing
                                    Just elem2 -> let prod = elem * elem2
                                                  in safeDivide prod 2
                                                  --changed to match firstTimesSecondOverTwo
firstOverSecondMonad :: Maybe Double
firstOverSecondMonad = 
  do elem <- safeHead lst
     tl <- safeTail lst
     elem2 <- safeHead tl
     safeDivide elem elem2

firstTimesSecond :: Maybe Double
firstTimesSecond =
  do elem <- safeHead lst
     tl <- safeTail lst
     elem2 <- safeHead tl
     let prod = elem * elem2
     safeDivide prod  2

assert True = Just ()
assert False = Nothing

fourthElem :: [Int] -> Maybe Int
fourthElem lst = 
  do hd <- safeHead lst
     assert (hd /= 0)
     tl <- safeTail lst
     tl2 <- safeTail tl
     tl3 <- safeTail tl2
     safeHead tl3

allProducts lstA lstB = 
  do a <- lstA
     b <- lstB
     return (a*b)
     return "HI!"

crossProduct ma mb =
  do a <- ma
     b <- mb
     return (a,b)

