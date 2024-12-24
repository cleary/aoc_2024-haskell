import System.IO
import Data.List
import Data.Text
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Text.Read (readMaybe)

parseTuple :: String -> Maybe (Int, Int)
parseTuple s = readMaybe s :: Maybe (Int, Int)

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- for a given list of strings xs, check if the char
-- at index x (column) y (row) equals a
checkChar :: [String] -> (Int, Int) -> Char -> Bool
checkChar xs (x,y) a = (xs !! y !! x) == a

main = do
    inputFile <- openFile "./input" ReadMode
    contents <- hGetContents inputFile
    -- concat multiple times to flatten lists
    let cl = Data.List.lines contents

    print (checkChar cl (0,1) 'A');
