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
import           System.Exit                    ( exitFailure )
import           System.IO
import           Types

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
    if not $ isSchemaStrValid schema
        then do
            putStrLn "Invalid schema specification, use the --help switch"
            exitFailure
        else do
            table <- parseCsv delimiter schema handle
            putStrLn ""
            prettyPrintTable table
            hClose handle

            loopUnlessMonad
                (\x -> map toLower x == "quit")
                getLine
                (\ln -> putStrLn "" >> printResult table (parseQuery ln))
  where
    isSchemaStrValid :: String -> Bool
    isSchemaStrValid = all (`elem` ['s', 'i', 'd', 'b'])

    printResult t (Just subqs) = printResult' (execute subqs (Right t))
    printResult _ Nothing      = putStrLn "cannot parse query"

    printResult' (Left  e) = putStrLn e
    printResult' (Right t) = prettyPrintTable t

