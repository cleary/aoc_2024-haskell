import System.IO
import Data.List

-- add numbers in a list
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

numTimesFound :: Ord a => a -> [a] -> Int
numTimesFound _ [] = 0
numTimesFound x xs = (length . filter (== x)) xs

ascending :: Ord a => a -> [a] -> Bool
ascending _ [] = True
ascending x xs = if x < (head xs) then ascending (head xs) (tail xs) else False

descending :: Ord a => a -> [a] -> Bool
descending _ [] = True
descending x xs = if x > (head xs) then descending (head xs) (tail xs) else False

gradual :: Int -> [Int] -> Bool
gradual _ [] = True
gradual x xs = if (abs (x - (head xs)) < 4) then gradual (head xs) (tail xs) else False


main = do
        inputFile <- openFile "./input" ReadMode
        contents <- hGetContents inputFile
        let splitWords = [ words x | x <- lines contents ];
        let splitNums =  [ map (read::String->Int) x | x <- splitWords ];
        let count = [ if (ascending (head x) (tail x)) == True then 
                (if (gradual (head x) (tail x)) == True then 1 else 0 )
            else if (descending (head x) (tail x)) == True then 
                (if (gradual (head x) (tail x)) == True then 1 else 0 )
            else 0 | x <- splitNums ];
        print (sumList count);
