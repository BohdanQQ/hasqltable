module Operations
    ( prettyPrintTable
    , execute
    ) where
import           Data.List                      ( elemIndex
                                                , groupBy
                                                , intercalate
                                                , sortBy
                                                , transpose
                                                )
import           Data.Maybe                     ( fromJust
                                                , isNothing
                                                )
import           Types

------------
--- PRINTING
------------

-- | returns string representation of a cell's value
strCellValue :: Cell -> String
strCellValue (CInt    i) = show i
strCellValue (CDouble i) = show i
strCellValue (CStr    i) = i
strCellValue (CBool   i) = show i

-- | prints the table with paddinng and column / header boundaries
prettyPrintTable :: Table -> IO ()
prettyPrintTable (schema, rowgroups) = do
    prettyPrintSchema schema colSizes
    mapM_ (`prettyPrintRow` colSizes) rows
  where
    schemaColLengths = map (length . fst) schema
    rows = concat rowgroups
    maxLenPerCol     = if null rows 
        then map (const 0) [1..length schema]  
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
prettyPrintSchema :: [(String, Cell)] -> [Int] -> IO ()
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

type QueryResult = Either String Table

------
--- MAIN QUERY EXECUTOR
------
-- | executes a query on a table
execute :: [SubQuery] -> QueryResult -> QueryResult
execute query wrappedTable = foldl exec wrappedTable reorderedQuery
  where
    -- reordering (for execution) is needed, see below
    reorderedQuery = reorderQuery query
    -- exec - execute subquery or propagate error
    exec (Right table) qry = executeSubquery table qry
    exec (Left  err  ) _   = Left err
    reorderQuery q = sortBy (\a b -> compare (idQ a) (idQ b)) q
    idQ :: SubQuery -> Int
    idQ (Select  _) = 10 -- select as last
    idQ (Limit   _) = 9
    idQ (OrderBy _) = 8 -- orderby before select (to ensure we can order by any column)
    idQ (GroupBy _) = 7
    idQ (Where   _) = 6


------
--- SELECT
------
-- | executes a single subquery on the table
executeSubquery :: Table -> SubQuery -> QueryResult
executeSubquery (schema, rowgroups) (Select cols) = if err0 == ""
    then Right (newSchema, extractedRows)
    else Left err0
  where
    -- the task here is to extract indicies of column names (cols) in the schema
    -- and filter cells from rows on those positions
    schemaCols       = map fst schema
    mbSchemaIndicies = zip cols (map (`elemIndex` schemaCols) cols)
    invalidCols      = map fst (filter (isNothing . snd) mbSchemaIndicies)
    err0             = if null invalidCols
        then ""
        else "Invalid columns (" ++ intercalate "," invalidCols ++ ") requested"
    schemaIndicies          = map (\(_, Just x) -> x) mbSchemaIndicies
    extractBySchemaIndicies = \x -> map (x !!) schemaIndicies
    newSchema               = extractBySchemaIndicies schema
    extractedRows           = map rowMapper rowgroups
    rowMapper rows = map extractBySchemaIndicies rows

------
--- LIMIT
------
executeSubquery (schema, rowgroups) (Limit rowCount) =
    Right (schema, [take (fromInteger rowCount) (concat rowgroups)])

------
--- ORDER BY
------

executeSubquery (schema, rowgroups) (OrderBy (order, columns)) =
    case sortedRows of
        (Right res) -> Right (schema, [res])
        (Left  err) -> Left err

  where
    comparer = case order of
        Asc  -> cellCompareAsc
        Desc -> cellCompareDesc
    schemaCols = map fst schema
    rows       = concat rowgroups
    sortedRows = sortRows rows columns schemaCols comparer

------
--- WHERE
------
executeSubquery (schema, rowgroups) (Where expr) = case result of
    Left  s            -> Left s
    Right filteredRows -> Right (schema, [filteredRows])

  where
    rows       = concat rowgroups
    resultRows = map
        (\r ->
            -- forcing evaulation with [boolAnd (Const (CBool True))]
            -- ensures that expr evaluates to a boolean expression
            (evalExpr schema r (Operation expr boolAnd (Const (CBool True))), r)
        )
        rows
    -- this propagates the first error (Left) into tmp
    tmp = foldl (\acc (e, _) -> acc >> e)
                (Right (CBool True) :: Either String Cell)
                resultRows
    result = case tmp of
        Left  err -> Left err
        Right _   -> Right
        -- finally get the rows (snd) for which the expression is true (okResult . fromRightUnsafe . fst)
            (map snd (filter (okResult . fromRightUnsafe . fst) resultRows))
    fromRightUnsafe x = case x of
        Right e -> e
        Left  _ -> error "Impossible error"
    okResult res = case res of
        (CBool x) -> x
        _         -> False

------
--- GROUP_BY
------

executeSubquery t@(schema, _) (GroupBy columns) = case groupedRows of
    Right  rg  -> Right (schema, [map head rg])
    Left err -> Left err
    where groupedRows = createGroupsBy t columns

-- | groups table rows by the specified columns 
createGroupsBy :: Table -> [String] -> Either String [[[Cell]]]
createGroupsBy (schema, rowGroups) columns = if null err0
    then sortedRows >>= (Right . Data.List.groupBy (sameOnIdcs colIdcs))
    else Left err0
  where
    -- pereparation, error checking
    schemaCols       = map fst schema
    sortedRows = sortRows rows columns schemaCols cellCompareDesc
    mbSchemaIndicies = zip columns (map (`elemIndex` schemaCols) columns)
    invalidCols      = map fst (filter (isNothing . snd) mbSchemaIndicies)
    err0             = if null invalidCols
        then ""
        else "Invalid columns " ++ intercalate "," invalidCols ++ " requested"
    colIdcs     = map (fromJust . snd) mbSchemaIndicies
    rows        = concat rowGroups
    -- compares two rows based on column values (idcs)
    sameOnIdcs idcs ra rb = all (\i -> (ra !! i) == (rb !! i)) idcs


sortRows :: Foldable t =>
    [Row]
    -> t String
    -> [String]
    -> (Cell -> Cell -> Either String Ordering)
    -> Either String [Row]
sortRows rows columns schemaCols comparer = foldr folder (Right rows) columns
    where
    folder column (Right cells) = orderByOne schemaCols column cells comparer
    folder  _ (Left  msg  )      = Left msg

-- | Orders rows by exactly one column, ordering is stable
orderByOne
    :: [String]
    -> String
    -> [Row]
    -> (Cell -> Cell -> Either String Ordering)
    -> Either String [Row]
orderByOne schema column rows comparer = if isNothing mbColIdx
    then Left $ "Could not find column " ++ column
    else Right sortedRows
  where
    -- sortBy is stable
    sortedRows = Data.List.sortBy
        (\r1 r2 -> case comparer (r1 !! colIdx) (r2 !! colIdx) of
            -- parsing error / table corruption, terminate
            Left s -> error
                ("Table parsed into invalid format! Attempted to compare incompatible types: "
                ++ s
                )
            Right ord -> ord
        )
        rows
    mbColIdx = elemIndex column schema
    colIdx   = fromJust mbColIdx
