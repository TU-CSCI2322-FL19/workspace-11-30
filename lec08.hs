import Debug.Trace

occurancesOfHead :: Eq a => [a] -> Int
occurancesOfHead [] = error "AAAH"
occurancesOfHead (x:xs) = count x (x:xs)
  where count :: Eq a => a -> [a] -> Int
        count y [] = 0
        count y (x:xs) 
          | x == y    = 1 + count y xs
          | otherwise = count y xs

--occurancesOfHead  [7,3,1,7,7,5] = 3

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = (rev xs) ++ [x]

frev :: Show a => [a] -> [a]
frev lst = 
  let aux [] acc     = traceShow (([] :: [Int]), acc) acc
      aux (x:xs) acc = traceShow (x,xs, acc) $ aux xs (x:acc)
  in aux lst []


gap :: [Int] -> Int
gap lst = maximum lst - minimum lst
