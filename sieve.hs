sieve [] = []
sieve (x:xs) = x:sieve (filter (\l -> mod l x /= 0) xs)