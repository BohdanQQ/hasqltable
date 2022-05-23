{-# LANGUAGE RecordWildCards #-}
module Main where
import           Config                         ( Config(..)
                                                , parseConfig
                                                )
import           Data.Char                      ( toLower )
import           Data.List
import           Data.List.Split
import           Operations                     ( execute
                                                , prettyPrintTable
                                                )
import           Options.Applicative
import           Parsing
import           System.Environment
import           System.IO
import           Types                          ( Cell(..)
                                                , Expr(..)
                                                , Order(..)
                                                , add
                                                , div
                                                , evalExpr
                                                , mul
                                                , sub
                                                , unwrapOrErr
                                                )

printResult t (Just subqs) = do
    printResult' (execute subqs (Right t))
printResult _ Nothing = putStrLn "cannot parse query"
printResult' (Left e) = do
    putStrLn e
printResult' (Right t) = do
    prettyPrintTable t

-- if fn is true, exits
-- monadfirst is the monad which gets a value
-- getMonad then gets another monad based on the value (when fn is false)
-- this repeats
loopUnlessMonad fn monadfirst getMonad =
    monadfirst
        >>= (\result -> if fn result
                then return ()
                else getMonad result >> loopUnlessMonad fn monadfirst getMonad
            )

opts :: ParserInfo Config
opts = info (parseConfig <* helper) (progDesc "Query a (so far only CSV) file")

main :: IO ()
main = do
    c@Config {..} <- execParser opts
    args          <- getArgs
    handle        <- openFile file ReadMode
    table         <- parseCsv delimiter schema handle
    putStrLn ""
    prettyPrintTable table
    hClose handle

    loopUnlessMonad
        (\x -> map toLower x == "quit")
        getLine
        (\ln -> putStrLn "" >> printResult table (parseQuery ln))

parseArgs args | argCount < 3 = error "Invalid Argument Count"
               | otherwise    = (head args, args !! 1, args !! 2)
    where argCount = length args
