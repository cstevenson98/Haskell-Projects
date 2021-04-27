import Data.List

main = do
        line <- getLine
        if null line 
        then return ()
        else do 
            putStrLn . (intercalate " ") $ (infixToRPN (words line) [])
            main

ops = ["+","-","/","*","^"]
par = ["(",")"]

infixToRPN :: [String] -> [String] -> [String]
infixToRPN [] [] = []
infixToRPN [] (x:xs) = [x] ++ infixToRPN [] xs

infixToRPN (x:xs) []
    | x `elem` ops   = infixToRPN xs [x]
    | otherwise      = [x] ++ infixToRPN xs []

infixToRPN (a:as) b@(bb:bbs)
    | a `elem` ops  = if (cI a < cI bb) 
                      then infixToRPN (bb:as) (a:bbs)
                      else [bb] ++ infixToRPN as (a:bbs)
    | a `elem` par  = undefined
    | otherwise     = [a] ++ infixToRPN as b


certainIndex :: String -> Int
certainIndex k 
    | k == "+"  = 2
    | k == "-"  = 1
    | k == "*"  = 3
    | k == "/"  = 3
    | k == "^"  = 4
    | otherwise = undefined

cI = certainIndex