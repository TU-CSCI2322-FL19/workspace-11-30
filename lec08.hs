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
gap [] = error "Gap of an empty list!"
gap lst = 
  let aux large small [] = large - small
      aux large small (x:xs) -- = aux (max x large) (min x small) xs
            | x < small = aux large x xs
            | x > large = aux x small xs
            | otherwise = aux large small xs
  in aux (head lst) (head lst) lst


range :: [Int] -> (Int, Int)
range [] = error "AAAAH"
range [x] = (x,x)
range (x:xs) = 
      let (large, small) = range xs
      in (max x large, min x small)

gap2 :: [Int] -> Int
gap2 lst = let (l,s) = range lst  in l-s

hoursToSchedule :: [(Int, Int)] -> Int
hoursToSchedule tasks = 
    let sumTimes :: [(Int, Int)] -> (Int, Int)
        sumTimes [] = (0,0)
        sumTimes ((h,m):ts) =
            let (hoursTs, minTs) = sumTimes ts
                totalHours = hoursTs + h
                totalMins = minTs + m
            in (totalHours + totalMins `div` 60, totalMins `mod` 60)
        (hours,mins) = sumTimes tasks
    in hours + if mins > 0 then 1 else 0
{-
hoursToSchedule ((h,m):ts) =
    let hoursForTs = hoursToSchedule ts
    in hoursForTs + h + if m > 0 then 1 else 0
    -}
