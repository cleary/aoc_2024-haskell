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

-- splitAtDo :: [a] -> [a]
splitAtDo :: [Text] -> [[Text]]
splitAtDo [] = []
splitAtDo xs = (discard, rest) = Data.List.splitAt (fromMaybe 0 (elemIndex "do()" xs)) xs
                 if length rest == 0 
                 then [] 
                 else (splitAtDont rest)

splitAtDont :: [Text] -> [[Text]]
splitAtDont [] = []
splitAtDont xs =   index <- fromMaybe 0 (elemIndex "do()" xs)
                   if index == 0
                   then xs
                   else (keep, rest) <- Data.List.splitAt index xs
                     keep ++ (splitAtDo rest) 


main = do
        inputFile <- openFile "./input" ReadMode
        contents <- hGetContents inputFile
        -- concat multiple times to flatten lists
        let muls = Data.List.concat (Data.List.concat ([ x =~ "(don't[(][)]|do[(][)]|mul[(][0-9]{1,3},[0-9]{1,3}[)])" :: [[String]] | x <- Data.List.lines contents ]));
        print (splitAtDo muls)
        -- let (headinc, tail) = Data.List.splitAt (fromMaybe 0 (elemIndex "don't()" muls)) muls;
        -- print (headinc)

        -- -- remove 'mul' from string and convert remainder to (Int, Int) tuple
        -- let  valPairs = [case parseTuple (unpack (replace (pack "mul") (pack "") ( pack x) )) of 
        --                   Just t -> t
        --                 | x <- muls ]
        -- -- muliply the pairs
        -- let mulVal = [ x * y | (x,y) <- valPairs ]
        -- print (sumList mulVal);
