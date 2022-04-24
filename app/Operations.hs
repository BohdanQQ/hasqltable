module Operations where
import           Types

------------
--- PRINTING (so far rather dump, using max width of all cells for each column)
------------

prettyPrintCell (CInt    i) = show i
prettyPrintCell (CDouble i) = show i
prettyPrintCell (CStr    i) = show i
prettyPrintCell (CBool   i) = show i

prettyPrintTable :: Table -> IO ()
prettyPrintTable (schema, rowgroups) = do
    prettyPrintSchema schema largestCol
    putStrLn ""
    mapM_ (`prettyPrintSingleGroup` largestCol) rowgroups
  where
    largestSchemaCol = maximum (map (length . fst) schema)
    largestRowsCol =
        maximum (map (length . prettyPrintCell) (concat (concat rowgroups)))
    largestCol = max largestRowsCol largestSchemaCol

paddTo padding what string = replicate (padding - length string) what ++ string

prettyPrintSchema schema padd =
    mapM_ ((\s -> putStr (paddTo padd ' ' s ++ "|")) . fst) schema

prettyPrintSingleGroup rows padd = mapM_ (prettyPrintRow padd) rows


prettyPrintRow padd row = do
    mapM_ (\cell -> putStr (paddTo padd ' ' (prettyPrintCell cell) ++ "|")) row
    putStrLn ""
