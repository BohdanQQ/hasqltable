module Operations
    ( prettyPrintTable
    , execute
    , tableFileSchema
    , tableToCsv
    ) where
import           Data.List                      ( elemIndex
                                                , groupBy
                                                , intercalate
                                                , sortBy
                                                , transpose
                                                )
import           Types
import Data.Function ((&))
import Data.Maybe (listToMaybe)

------------
--- PRINTING
------------

-- | returns string representation of a cell's value
strCellValue :: Cell -> String
strCellValue (CInt    i) = show i
strCellValue (CDouble i) = show i
strCellValue (CStr    i) = i
strCellValue (CBool   i) = show i

-- | returns string representation of a cell's type (for schema)
strCellSchemaSpec :: SchemaType -> String
strCellSchemaSpec SInt    = "i"
strCellSchemaSpec SDouble = "d"
strCellSchemaSpec SStr    = "s"
strCellSchemaSpec SBool   = "b"

-- | prints the table with paddinng and column / header boundaries
prettyPrintTable :: Table -> IO ()
prettyPrintTable (schema, rowgroups) = do
    prettyPrintSchema schema colSizes
    mapM_ (`prettyPrintRow` colSizes) rows
  where
    schemaColLengths = map (length . fst) schema
    rows             = concat rowgroups
    maxLenPerCol     = if null rows
        then map (const 0) [1 .. length schema]
        else map (maximum . map (length . strCellValue)) (transpose rows)
    colSizes = zipWith max maxLenPerCol schemaColLengths
-- | applies padding to the end of a string to match certain length
padTo :: Int -> Char -> String -> String -> String
padTo padding padChar strToPad extraStr =
    extraStr
        ++ strToPad
        ++ replicate (padding - length strToPad) padChar
        ++ extraStr


-- | prints the schema row and a separatring line
prettyPrintSchema :: Schema -> [Int] -> IO ()
prettyPrintSchema schema padding = do
    putStrLn $ intercalate
        "|"
        (zipWith (\s p -> padTo p ' ' (fst s) " ") schema padding)
    -- separating line is the same, except instead of column name, the - is repeated (lenght name)-times
    putStrLn $ intercalate
        "+"
        (zipWith (\s p -> padTo p '-' (replicate (length (fst s)) '-') "-")
                 schema
                 padding
        )

-- | prints a row with paddinng and column boundaries
prettyPrintRow :: Row -> [Int] -> IO ()
prettyPrintRow row padding = putStrLn $ intercalate
    "|"
    (zipWith (\cell pad -> padTo pad ' ' (strCellValue cell) " ") row padding)

------
--- FILE SAVING
------
-- | returns the schema of the table which can be loaded by this program
tableFileSchema :: Table -> String
tableFileSchema (schema, _) = concatMap (strCellSchemaSpec . snd) schema

-- | returns the csv representation of a table
tableToCsv :: Table -> String
tableToCsv (schema, rows) =
    let
        schemarow   = intercalate "," (map fst schema) ++ "\n"
        stringRows  = intercalate "\n" $ concat rows & map (intercalate "," . map strCellValue)
    in
        schemarow ++ stringRows


type QueryResult = Either String Table

------
--- MAIN QUERY EXECUTOR
------
-- | executes a query on a table
execute :: [SubQuery] -> Table -> QueryResult
execute query tbl = foldl exec (Right tbl) reorderedQuery
  where
    -- reordering (for execution) is needed, see below
    reorderedQuery = reorderQuery query
    -- exec - execute subquery or propagate error
    exec (Right table) qry = executeSubquery table qry
    exec (Left  err  ) _   = Left err
    reorderQuery = sortBy (\a b -> compare (idQ a) (idQ b))
    idQ :: SubQuery -> Int
    idQ (Select  _) = 10 -- select as last
    idQ (Limit   _) = 9
    idQ (OrderBy _) = 8 -- orderby before select (to ensure we can order by any column)
    idQ (GroupBy _) = 7
    idQ (Where   _) = 6

notInSchemaErr :: [Char] -> [Char]
notInSchemaErr column = "column " ++ column ++ " not found in schema"

checkedColumnIndicies :: Schema -> [String] -> Either String [Int] 
checkedColumnIndicies schema cols = do
    let schemaCols       = map fst schema
    mapM (\x -> case elemIndex x schemaCols of
        Nothing -> Left $ notInSchemaErr x
        Just i -> Right i
      ) cols

------
--- SELECT
------
-- | executes a single subquery on the table
executeSubquery :: Table -> SubQuery -> QueryResult
executeSubquery (schema, rowgroups) (Select cols) = do
    -- the task here is to extract indicies of column names (cols) in the schema
    -- and filter cells from rows on those positions
    colIndicies <- checkedColumnIndicies schema cols
    let extractBySchemaIndicies row = map (row !!) colIndicies
        newSchema                   = map (schema !!) colIndicies
        extractedRows               = map (map extractBySchemaIndicies) rowgroups
    Right (newSchema, extractedRows)


------
--- LIMIT
------
executeSubquery (schema, rowgroups) (Limit rowCount) =
    Right (schema, [take (fromInteger rowCount) (concat rowgroups)])

------
--- ORDER BY
------

executeSubquery (schema, rowgroups) (OrderBy (order, columns)) = do
    let schemaCols = map fst schema
        rows       = concat rowgroups
    sorted <- sortRows rows columns schemaCols comparer
    Right (schema, [sorted])
  where
    comparer = case order of
        Asc  -> cellCompareAsc
        Desc -> cellCompareDesc


------
--- WHERE
------
executeSubquery (schema, rowgroups) (Where expr) = do
    evaluated <- concat rowgroups & mapM
            (\r ->
                -- forcing evaulation with [boolAnd (Const (CBool True))]
                -- ensures that expr evaluates to a boolean expression
                do 
                    res <- evalExpr schema r (Operation expr boolAnd (Const (CBool True)))
                    Right (res, r)
            )
    let filteredRows = map snd (filter (okResult . fst) evaluated)
    Right (schema, [filteredRows])
  where
    okResult res = case res of
        (CBool x) -> x
        _         -> False

------
--- GROUP_BY
------

executeSubquery t@(schema, _) (GroupBy columns) = do
    rg <- createGroupsBy t columns
    heads <- rg & mapM (\x -> listToMaybe x & maybeToRight "Unexpected empty grouped rows")
    Right (schema, [heads])

-- | groups table rows by the specified columns 
createGroupsBy :: Table -> [String] -> Either String [RowGroup]
createGroupsBy (schema, rowGroups) cols = do
    colIndicies <- checkedColumnIndicies schema cols
    let 
        schemaCols = map fst schema
    sortedRows <- sortRows rows cols schemaCols cellCompareDesc
    Right (Data.List.groupBy (sameOnIdcs colIndicies) sortedRows)
  where
    -- pereparation, error checking
    rows    = concat rowGroups
    -- compares two rows based on column values (idcs)
    sameOnIdcs idcs ra rb = all (\i -> (ra !! i) == (rb !! i)) idcs

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight err x = case x of
    Just y -> Right y
    Nothing -> Left err


sortRows :: [Row]
    -> [String]
    -> [String]
    -> (Cell -> Cell -> Either String Ordering)
    -> Either String [Row]
sortRows rows columns schemaCols comparer = foldr 
        (\column acc -> do
            _ <- acc
            colIdx <- elemIndex column schemaCols & maybeToRight (notInSchemaErr column)
            Right $ sortedAt colIdx
        )
        (Right rows) columns
    where
    sortedAt idx =
      rows
        & Data.List.sortBy
          ( \r1 r2 -> case comparer (r1 !! idx) (r2 !! idx) of
              -- parsing error / table corruption, terminate
              Left s ->
                error
                  ( "Table parsed into invalid format! Attempted to compare incompatible types: "
                      ++ s
                  )
              Right ord -> ord
          )