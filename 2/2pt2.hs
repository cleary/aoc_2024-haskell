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

doubleCheckSafe :: [Int] -> Int -> Bool
doubleCheckSafe [] _ = False
doubleCheckSafe xs n = if safe (let (ys,zs) = splitAt n xs in ys ++ (tail zs)) == False then
                           if (n + 1) >= length xs then False else doubleCheckSafe xs (n + 1)
                       else True

safe :: [Int] -> Bool
safe [] = False
safe xs = if (ascending (head xs) (tail xs)) == True then 
                (if (gradual (head xs) (tail xs)) == True then True else False )
            else if (descending (head xs) (tail xs)) == True then 
                (if (gradual (head xs) (tail xs)) == True then True else False )
            else False

main = do
        inputFile <- openFile "./input" ReadMode
        contents <- hGetContents inputFile
        let splitNums = [map (read::String->Int) (words x) | x <- lines contents ];
        let count = [ if safe x == True then 1 
            else if doubleCheckSafe x 0 == True then 1 
            else 0 | x <- splitNums ];
        print (sumList count);
