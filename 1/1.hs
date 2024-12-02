import System.IO
import Data.List

-- add numbers in a list
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

main = do
        inputFile <- openFile "./input" ReadMode
        contents <- hGetContents inputFile
        let splitWords = [ words x | x <- lines contents ];
        let left = [ head x | x <- splitWords ];
        let right = [ last x | x <- splitWords ];
        -- sort and convert Str to Int
        let sortLeft = map (read::String->Int) (sort left);
        let sortRight = map (read::String->Int) (sort right);
        let subtractList = [ if x > y then x - y else y - x | (x,y) <- zip sortLeft sortRight ];
        print (sumList subtractList);
        hClose inputFile
