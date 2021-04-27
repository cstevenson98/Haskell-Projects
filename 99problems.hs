-- Problem 1
mylast :: [a] -> a
mylast = head . reverse

-- Problem 2
myButLast :: [a] -> a
myButLast list = reverse list !! 1

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt list k = list !! (k - 1)

-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = (reverse x) == x

-- Problem 7
-- (Flatten a nested list)

-- Nested list data type:
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

flatten' :: NestedList a -> [a] 
flatten' (Elem x) = [x]
flatten' (List x) = concatMap flatten x

-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress' :: (Eq a) => a -> [a] -> [a]
compress (x:xs) = compress' x xs
compress' p [] = [p]
compress' p (x:xs)
    | p == x        = compress' p xs
    | p /= x        = [p] ++ compress' x xs

-- Problem 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = [takeWhile (==x) (x:xs)] ++ pack (dropWhile (==x) (x:xs))

-- Problem 10 
encode :: (Eq a) => [a] -> [(Int, a)]
encode = (map (\l -> (length l, head l))) . pack

-- Problem 11
data Entry a = Single a | Multiple Int a
    deriving(Show)
encodeModified :: (Eq a) => [a] -> [Entry a]
encodeModified = (map (\l -> if (length l == 1) then Single (head l)
                             else                    Multiple (length l) (head l))) . pack

-- Problem 12 

entry2list :: Entry a -> [a] 
entry2list (Single x) = [x]
entry2list (Multiple num x) = take num (repeat x)

decodeModified :: (Eq a) => [Entry a] -> [a]
decodeModified = concat . (map entry2list)

-- Problem 13
-- Do it w/out pack

-- Problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

-- Problem 15
repli :: Int -> [a] -> [a]
repli n = concat . (map (\x -> take n (repeat x)))
