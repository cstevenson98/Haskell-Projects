module LeafyTrees
(
    LeafyTree
) where

import Control.Monad


-- LeafyTree data type. A general monadic binary tree type.
data LeafyTree a = Leaf a | Branch a (LeafyTree a) (LeafyTree a) deriving(Show)

instance Monad LeafyTree where
    return = Leaf 

-- instance Functor LeafyTree where
--     fmap f Leaf a = Leaf a
--     fmap f (Branch a left right) = Branch (f a) (fmap f left) (fmap f right)

-- instance Applicative LeafyTree where
--     pure a = Branch a Leaf a Leaf a
--     Leaf a <*> _ = Leaf a
--     _ <*> Leaf a = Leaf a
--     (Branch f left right) <*> (Branch x xleft xright) 
--         = Branch (f x) (left <*> xleft) (right <*> xright) 