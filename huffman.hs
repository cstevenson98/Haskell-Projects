import Data.List
import Control.Monad
import qualified Data.Map as M
import System.Environment
-- Definition of the tree data type:

data Tree a = Empty | Node a (Tree a) (Tree a) 
    deriving(Show)

instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

instance Applicative Tree where
    pure x = Node x Empty Empty
    Empty <*> _ = Empty 

-- Counts number of each unique character in a given string, and returns this data in a list of Trees.

collect :: String -> [Tree (Int, Char)]
collect a = map (\l -> Node ((length .) (filter (==l)) a, l) Empty Empty) (nub a)

-- Given a list of Trees, will order them based on the contents of its sub-Trees.
forestSort :: Ord a => [Tree (a, t)] -> [Tree (a, t)]
forestSort = sortBy (\(Node (x,_) _ _) (Node (y,_) _ _) -> compare x y)

-- Given a list of Trees, returns the Huffman Tree associated with the set of characters contained in the Trees.
buildTree :: (Ord a, Num a) => [Tree (a, Char)] -> Tree (a, Char)
buildTree (t1@(Node (x,_) _ _):t2@(Node (y,_) _ _):xs) = 
    buildTree (forestSort (Node (x+y,'§') t1 t2 :xs))
buildTree [x] = x

-- Displays a list of Booleans as a string of '0's and '1's.
binRep :: [Bool] -> Tree (t, Char) -> [(Char, [Bool])]
binRep bitstring (Node (_, ms) Empty Empty) = [(ms, bitstring)]
binRep bitstring (Node _ left right) = binRep (bitstring ++ [False]) left 
                                    ++ binRep (bitstring ++ [True]) right 

binToString :: [Bool] -> [Char]
binToString [] = []
binToString (x:xs) = (if x then "1" else "0") ++ binToString xs

huffEncode = M.fromList . (binRep [] ) . buildTree . forestSort . collect