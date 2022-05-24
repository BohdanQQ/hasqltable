{-# LANGUAGE RecordWildCards #-}
module Main where
import           Config                         ( Config(..)
                                                , parseConfig
                                                )
import           Data.Char                      ( toLower )
import           Operations                     ( execute
                                                , prettyPrintTable
                                                )
import           Options.Applicative
import           Parsing
import           System.Exit                    ( exitFailure )
import           System.IO

-- | an monadic loop with terminating condition
--
-- if fn is true, exits
--
-- monadfirst - the monad which gets a value
--
-- getMonad then gets another monad based on the value (when fn is false)
-- and all of this repeats until fn is false
loopUnlessMonad :: Monad m => m t -> (t -> Bool) -> (t -> m a) -> m ()
loopUnlessMonad monadfirst fn getMonad =
    monadfirst
        >>= (\result -> if fn result
                then return ()
                else getMonad result >> loopUnlessMonad monadfirst fn getMonad
            )

main :: IO ()
main = do
    Config {..} <- execParser opts
    handle      <- openFile file ReadMode
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
                getLine
                (\x -> map toLower x == "quit")
                (\ln -> putStrLn "" >> printResult table (parseQuery ln))
  where
    isSchemaStrValid :: String -> Bool
    isSchemaStrValid = all (`elem` ['s', 'i', 'd', 'b'])

    printResult t (Just subqs) = printResult' (execute subqs (Right t))
    printResult _ Nothing      = putStrLn "cannot parse query"

    printResult' (Left  e) = putStrLn e
    printResult' (Right t) = prettyPrintTable t

    opts =
        info (parseConfig <* helper) (progDesc "Query a (so far only CSV) file")

