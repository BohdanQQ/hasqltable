{-# LANGUAGE RecordWildCards #-}
module Main where
import           Config                         ( Config(..)
                                                , parseConfig, SchemaInput (File, Raw)
                                                )
import           Data.Char                      ( toLower )
import           Operations                     ( execute
                                                , prettyPrintTable
                                                , tableFileSchema
                                                , tableToCsv
                                                )
import           Options.Applicative
import           Parsing
import           System.Exit                    ( exitFailure )
import           System.IO
import           Types                          ( Table )

-- | the implementation of the REPL
repl :: IO [Char] -> Table -> Table -> IO ()
repl inputMonad table originalTable = do
    input <- inputMonad
    let lwrRes = map toLower input
    if lwrRes == "quit" || lwrRes == "exit"
        then return ()
    else if lwrRes == "save"
        then do
        putStrLn "Enter name (for csv table and schema file - named the same with .schema added):"
        fileName <- getLine
        writeFile fileName (tableToCsv table)
        writeFile (fileName ++ ".schema")
                    (tableFileSchema table)
        putStrLn
            $  "Saved into "
            ++ fileName
            ++ " and  "
            ++ (fileName ++ ".schema")
        repl inputMonad table originalTable
    else if lwrRes == "reset" || lwrRes == "reload"
        then do
        printTableOrErr $ Right originalTable
        repl inputMonad originalTable originalTable
    else do
        let modified = go table input
        printTableOrErr modified
        let tbl = retOrElse table modified
        repl inputMonad tbl originalTable
  where
    go target ln = newTable
      where
        mbQry    = parseQuery ln
        newTable = executeWithErr target mbQry

        executeWithErr t (Just subqs) = execute subqs t
        executeWithErr _ Nothing      = Left "Cannot parse query"
    retOrElse elseVal (Left _) = elseVal
    retOrElse _       (Right val)    = val
    printTableOrErr (Left  err  ) = putStrLn err
    printTableOrErr (Right t)     = prettyPrintTable t

main :: IO ()
main = do
    Config {..} <- execParser opts
    handle      <- openFile file ReadMode
    schema <- case schemaSpec of
      Raw s -> return s
      File s -> readFile s
    if not $ isSchemaStrValid schema
        then do
            putStrLn "Invalid schema specification, use the --help switch"
            exitFailure
        else do
            table <- parseCsv delimiter schema handle
            putStrLn ""
            prettyPrintTable table
            hClose handle

            repl getLine table table
  where
    isSchemaStrValid :: String -> Bool
    isSchemaStrValid = all (`elem` ['s', 'i', 'd', 'b'])


    opts             = info
        (parseConfig <* helper)
        (progDesc
            "Query a (so far only CSV) file. The program runs in a REPL. To quit, type `quit` or `exit` as the command. To save current table or reset to the initial table, type save or reset respectively."
        )

