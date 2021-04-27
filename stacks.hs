module Stacks
( Stack
 ,pop
 ,push
) where

-- We implement a stack as a list of some type, with a push function and pop function.

data Stack a = Empty | Head a [a] deriving(Show)

pop :: Stack a -> (a, Stack a)
pop Empty             = (undefined, Empty)
pop (Head x [])       = (x, Empty)
pop (Head x (xx:xxs)) = (x, Head xx xxs) 

push :: a -> Stack a -> Stack a
push a Empty       = Head a []
push a (Head x xx) = Head a (x:xx)