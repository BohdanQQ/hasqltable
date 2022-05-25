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
-- getMonad then returns another monad based on the value returned by monadFirst and the initial value (when fn is false)
--
-- this monad also returns a new value which is used in the next loop
-- and all of this repeats until fn is false
loopUnlessMonad
    :: Monad m => m t -> (t -> Bool) -> (t -> a -> m a) -> a -> m ()
loopUnlessMonad monadfirst fn getMonad initial =
    monadfirst
        >>= (\result -> if fn result
                then return ()
                else
                    getMonad result initial
                        >>= loopUnlessMonad monadfirst fn getMonad
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
                getLine                                         -- get input
                (\x -> map toLower x == "quit" || x == "exit")  -- terminating condition
                (\ln val ->
                    putStrLn ""
                        >>  return (go val ln)                  -- evaluate query with a table    
                        >>= (\ret -> printTableOrErr ret >> retOrElse val ret) -- print result and propagate proper table into next iteration
                )                                                              -- proper here means the LAST VALID (Right table) result!!!
                (Right table)
  where
    -- propagates first argument if the second is Left
    retOrElse elseVal (Left _) = return elseVal
    retOrElse _       ifVal    = return ifVal

    printTableOrErr (Left  err  ) = putStrLn err
    printTableOrErr (Right table) = prettyPrintTable table

    -- handles BOTH query parsing and query execution
    -- wraps errors into a Left
    -- correct table into a Right
    go (Left e) _  = Left e
    go table    ln = newTable
      where
        mbQry    = parseQuery ln
        newTable = executeWithErr table mbQry

        executeWithErr t (Just subqs) = execute subqs t
        executeWithErr _ Nothing      = Left "Cannot parse query"


    isSchemaStrValid :: String -> Bool
    isSchemaStrValid = all (`elem` ['s', 'i', 'd', 'b'])


    opts             = info
        (parseConfig <* helper)
        (progDesc
            "Query a (so far only CSV) file. The program runs in a REPL. To quit, type `quit` or `exit` as the command."
        )

