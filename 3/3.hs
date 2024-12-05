import System.IO
import Data.List
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

main = do
        inputFile <- openFile "./input" ReadMode
        contents <- hGetContents inputFile
        let output = [ x =~ "mul[(][0-9]{1,3},[0-9]{1,3}[)]" :: String | x <- lines contents ];
        print output;
