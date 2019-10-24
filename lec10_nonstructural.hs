import Debug.Trace


quickSort [] = []
quickSort [x] = [x]
quickSort (pivot:xs) = 
  let earliers = [x | x <- xs, x <= pivot]
      laters   = [x | x <- xs, x > pivot]
      sortedEarliers = quickSort earliers
      sortedLaters = quickSort laters
  in sortedEarliers ++ [pivot] ++ sortedLaters

hanoi :: Int -> Char -> Char -> Char -> [(Int, Char, Char)]
hanoi 0 source middle dest = []
hanoi n source middle dest =
    let firstStep = hanoi (n-1) source dest middle
        middleMove = (n, source, dest)
        lastStep = hanoi (n-1) middle source dest
    in firstStep ++ [middleMove] ++ lastStep
