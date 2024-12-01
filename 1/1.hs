import System.IO
import Control.Monad

main = do
        inputFile <- openFile "./input" ReadMode
        contents <- hGetContents inputFile
        lines contents
        hClose inputFile

f :: [String] -> [Int]
f = map read
