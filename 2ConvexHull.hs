import System.Environment (getArgs)


-- Packaging the idea of acting on input,
-- producing output
interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function inputFile)

-- Some function 'myFunction' will run the main stuff
main = mainWith myFunction
    where 
        mainWith function = do 
            -- Collect command line arguments
            args <- getArgs
            case args of 
                [input,output] -> interactWith function input output
                _ -> putStrLn "error: exactly two arguments needed"
        
        -- PLACEHOLDER: 
        myFunction = unlines . toCSV . convexHull . toCoords . lines 

toCoords :: [String] -> [(Double, Double)]
toCoords = undefined

toCSV :: [(Double, Double)] -> [String]
toCSV = undefined

convexHull :: [(Double, Double)] -> [(Double, Double)]
convexHull = undefined

data Direction = L | S | R
    deriving(Show)

turn :: (Double, Double) -> (Double, Double) -> (Double, Double) 
            -> Direction
turn (x1, y1) (x2, y2) (x3, y3)
    | crossZ <= 0.0 = R
    | crossZ == 0.0 = S
    | crossZ >= 0.0 = L
        where
            crossZ = (x2-x1)*(y3-y1) - (x3-x1)*(y2-y1)

turnList (x:xx:xxx:xs) = [turn x xx xxx] ++ turnList (xx:xxx:xs)
turnList (x:xx:[]) = []