module Main where
import           Control.Monad                  ( forever )
import           Data.Char
import           Data.List
import           Data.List.Split
import           Operations                     ( execute
                                                , parseCsv
                                                , parseQuery
                                                , prettyPrintTable
                                                )
import           System.Environment
import           System.IO
import           Types                          ( Cell(..)
                                                , Expr(..)
                                                , Order(..)
                                                , add
                                                , div
                                                , evalExpr
                                                , mul
                                                , parseCell
                                                , sub
                                                , typeOfCell
                                                , unwrapOrErr
                                                )

printResult t (Just subqs) = do
    print subqs
    printResult' (execute subqs (Right t))
printResult _ Nothing = putStrLn "cannot parse query"
printResult' (Left e) = do
    putStrLn e
printResult' (Right t) = do
    prettyPrintTable t

loopUntilMonad fn monadfirst getMonad =
    monadfirst
        >>= (\result -> if fn result
                then return "end of loop"
                else getMonad result >> loopUntilMonad fn monadfirst getMonad
            )

main :: IO ()
main = do
    args <- getArgs
    let (path, schemaDesc, parserProperty) = parseArgs args
    handle <- openFile path ReadMode
    table  <- parseCsv parserProperty schemaDesc handle
    putStrLn ""
    prettyPrintTable table

    loopUntilMonad
        (\x -> map toLower x == "quit")
        getLine
        (\ln -> putStrLn "RESULT" >> printResult table (parseQuery ln))
    hClose handle

parseArgs args | argCount < 3 = error "Invalid Argument Count"
               | otherwise    = (head args, args !! 1, args !! 2)
    where argCount = length args
