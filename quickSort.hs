quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) 
    = quickSort (filter (<x) xs) ++ [x] ++ quickSort (filter (>=x) xs)