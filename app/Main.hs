module Main where
import           Data.List
import           Data.List.Split
import           Operations                     ( parseCsv
                                                , prettyPrintTable
                                                )
import           System.Environment
import           System.IO
import           Types                          ( Cell(..)
                                                , parseCell
                                                , typeOfCell
                                                , unwrapOrErr
                                                )

main :: IO ()
main = do
    args <- getArgs
    let (path, schemaDesc, parserProperty) = parseArgs args
    handle <- openFile path ReadMode
    table  <- parseCsv parserProperty schemaDesc handle
    putStrLn ""
    prettyPrintTable table
    hClose handle

parseArgs args | argCount < 3 = error "Invalid Argument Count"
               | otherwise    = (head args, args !! 1, args !! 2)
    where argCount = length args
