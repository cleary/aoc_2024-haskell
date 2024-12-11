import System.IO
import Data.List
import Data.Maybe (fromMaybe)
import Data.Text
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Text.Read (readMaybe)

parseTuple :: String -> Maybe (Int, Int)
parseTuple s = readMaybe s :: Maybe (Int, Int)

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

splitAtDo :: [String] -> [String]
splitAtDo [] = []
splitAtDo xs = case (Data.List.splitAt (fromMaybe 0 (elemIndex "do()" xs))) xs of
                 ([], _) -> []
                 (discard, rest) -> splitAtDont (Data.List.tail rest)

splitAtDont :: [String] -> [String]
splitAtDont [] = []
splitAtDont xs = case (Data.List.splitAt (fromMaybe 0 (elemIndex "don't()" xs))) xs of
                   ([], keep) -> keep
                   (keep, rest) -> keep ++ (splitAtDo (Data.List.tail rest))


main = do
        inputFile <- openFile "./input" ReadMode
        contents <- hGetContents inputFile
        -- concat multiple times to flatten lists
        let muls = Data.List.concat (Data.List.concat ([ x =~ "don't[(][)]|do[(][)]|mul[(][0-9]{1,3},[0-9]{1,3}[)]" :: [[String]] | x <- Data.List.lines contents ]));
        -- remove 'mul' from string and convert remainder to (Int, Int) tuple
        let  valPairs = [case parseTuple (unpack (replace (pack "mul") (pack "") ( pack x) )) of 
                      Just t -> t
                      Nothing -> (0,0)
                    | x <- splitAtDont muls ]
        -- muliply the pairs
        let mulVal = [ x * y | (x,y) <- valPairs ]
        print (sumList mulVal);
