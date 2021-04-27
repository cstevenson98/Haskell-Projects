-- Code to do matrix algebra in Haskell.


-- Matrix data type, and error messages 	
data Matrix a = Empty |  Mtx (Int, Int) [[a]] | Mte String
    deriving(Show)

del x xp 
  | x /= xp = 0
  | otherwise = 1

identity n = Mtx (n, n) [[del x y | x <- [1..n]] | y <- [1..n]]

contents1 = [[i*j | i <- [2,4..8]] | j <- [1,3..9]]
contents2 = [[i*j | i <- [2,4..10]] | j <- [1,3..7]]
m1 = Mtx (5,4) contents1
m2 = Mtx (4,5) contents2

-- This is the basic 
-- matrixproduct :: Matrix -> Matrix -> Matrix

col (Mtx (x, y) cont) selection = [i !! selection | i <- cont]
cols m@(Mtx (x, y) cont) = [col m i | i <- [0..y-1]]

rows (Mtx (x, y) cont) = cont

matrixproduct _ (Mte a) = (Mte a)
matrixproduct (Mte a) _ = (Mte a)
matrixproduct a@(Mtx (aX, aY) aCont) b@(Mtx (bX, bY) bCont) 
  | aY /= bX = Mte "Error"
  | otherwise = Mtx (aX, bY) [[sum [(n !! k)*(m !! k) | k <- [0..aY-1]] | n <- cols b] | m <- rows a]
