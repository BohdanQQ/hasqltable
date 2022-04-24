module Operations where
import           Data.List                      ( transpose )
import           Types

------------
--- PRINTING
------------

prettyPrintCell (CInt    i) = show i
prettyPrintCell (CDouble i) = show i
prettyPrintCell (CStr    i) = i
prettyPrintCell (CBool   i) = show i

prettyPrintTable :: Table -> IO ()
prettyPrintTable (schema, rowgroups) = do
    prettyPrintSchema schema colSizes
    putStrLn ""
    mapM_ (`prettyPrintSingleGroup` colSizes) rowgroups
  where
    schemaColLengths = map (length . fst) schema
    maxLenPerCol     = map (maximum . map (length . prettyPrintCell))
                           (transpose (concat rowgroups))
    colSizes = zipWith max maxLenPerCol schemaColLengths

padTo :: Int -> Char -> String -> String
padTo padding what string =
    " " ++ string ++ replicate (padding - length string) what ++ " "


prettyPrintSchema :: [(String, Cell)] -> [Int] -> IO ()
prettyPrintSchema schema padding =
    mapM_ (\(s, p) -> putStr (padTo p ' ' (fst s) ++ "|")) (zip schema padding)

prettyPrintSingleGroup :: [Row] -> [Int] -> IO ()
prettyPrintSingleGroup rows padding = mapM_ (prettyPrintRow padding) rows

prettyPrintRow :: [Int] -> Row -> IO ()
prettyPrintRow padding row = do
    mapM_
        (\(cell, pad) -> putStr (padTo pad ' ' (prettyPrintCell cell) ++ "|"))
        (zip row padding)
    putStrLn ""
