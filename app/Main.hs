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
repl :: IO [Char] -> Either String Table -> Table -> IO ()
repl firstMonad initial resetTable = do
    fstRes <- firstMonad
    let lwrRes = map toLower fstRes
    if lwrRes == "quit" || lwrRes == "exit"
        then return ()
    else if lwrRes == "save"
        then do
        putStrLn "Enter name (for csv table and schema file - named the same with .schema added):"
        fileName <- getLine
        writeFile fileName (tableToCsv (unwrapTable initial))
        writeFile (fileName ++ ".schema")
                    (tableFileSchema (unwrapTable initial))
        putStrLn
            $  "Saved into "
            ++ fileName
            ++ " and  "
            ++ (fileName ++ ".schema")
        repl firstMonad initial resetTable
    else if lwrRes == "reset" || lwrRes == "reload"
        then do
        printTableOrErr (Right resetTable)
        repl firstMonad (Right resetTable) resetTable
        else do
        let parsed = go initial lwrRes
        printTableOrErr parsed
        repl firstMonad (retOrElse initial parsed) resetTable
  where
    go (Left e) _  = Left e
    go table    ln = newTable
      where
        mbQry    = parseQuery ln
        newTable = executeWithErr table mbQry

        executeWithErr t (Just subqs) = execute subqs t
        executeWithErr _ Nothing      = Left "Cannot parse query"
    retOrElse elseVal (Left _) = elseVal
    retOrElse _       ifVal    = ifVal

    unwrapTable (Right table) = table
    unwrapTable _             = undefined

    printTableOrErr (Left  err  ) = putStrLn err
    printTableOrErr (Right table) = prettyPrintTable table

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

            repl getLine (Right table) table
  where
    isSchemaStrValid :: String -> Bool
    isSchemaStrValid = all (`elem` ['s', 'i', 'd', 'b'])


    opts             = info
        (parseConfig <* helper)
        (progDesc
            "Query a (so far only CSV) file. The program runs in a REPL. To quit, type `quit` or `exit` as the command. To save current table or reset to the initial table, type save or reset respectively."
        )

