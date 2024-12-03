import System.IO
import Data.List

-- add numbers in a list
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

numTimesFound :: Ord a => a -> [a] -> Int
numTimesFound _ [] = 0
numTimesFound x xs = (length . filter (== x)) xs

main = do
        inputFile <- openFile "./input" ReadMode
        contents <- hGetContents inputFile
        let splitWords = [ words x | x <- lines contents ];
        let left = map (read::String->Int) [ head x | x <- splitWords ];
        let right = map (read::String->Int) [last x | x <- splitWords ];
        let matchList = [ x * (numTimesFound x right) | x <- left ];
        print (sumList matchList);
        hClose inputFile
