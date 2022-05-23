module Operations
    ( prettyPrintTable
    , execute
    ) where
import           Data.Char                      ( isSpace
                                                , toLower
                                                )
import           Data.List                      ( elemIndex
                                                , groupBy
                                                , intercalate
                                                , intersperse
                                                , nub
                                                , sortBy
                                                , transpose
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Maybe
import qualified GHC.Unicode                   as Char
import           Parser
import           System.IO                      ( Handle
                                                , hGetContents
                                                )
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
    mapM_ (`prettyPrintSingleGroup` colSizes) rowgroups
  where
    schemaColLengths = map (length . fst) schema
    maxLenPerCol     = map (maximum . map (length . prettyPrintCell))
                           (transpose (concat rowgroups))
    colSizes = zipWith max maxLenPerCol schemaColLengths

padTo :: Int -> Char -> String -> String -> String
padTo padding what string extra =
    extra ++ string ++ replicate (padding - length string) what ++ extra


prettyPrintSchema :: [(String, Cell)] -> [Int] -> IO ()
prettyPrintSchema schema padding = do
    putStrLn $ intercalate
        "|"
        (zipWith (\s p -> padTo p ' ' (fst s) " ") schema padding)
    putStrLn $ intercalate
        "+"
        (zipWith (\s p -> padTo p '-' (replicate (length (fst s)) '-') "-")
                 schema
                 padding
        )


prettyPrintSingleGroup :: [Row] -> [Int] -> IO ()
prettyPrintSingleGroup rows padding = mapM_ (prettyPrintRow padding) rows

prettyPrintRow :: [Int] -> Row -> IO ()
prettyPrintRow padding row = putStrLn $ intercalate
    "|"
    (zipWith (\cell pad -> padTo pad ' ' (prettyPrintCell cell) " ") row padding
    )

type QueryResult = Either Table String


------
--- MAIN QUERY EXECUTOR
------
-- executes a query on a table
execute
    :: [SubQuery]
    -> Either String ([(String, Cell)], [[[Cell]]])
    -> Either String ([(String, Cell)], [[[Cell]]])
execute query table = foldl exec table reorderedQuery
  where
    reorderedQuery = reorderQuery query
    exec (Right table) qry = executeQuery table qry
    exec (Left  error) _   = Left error
    reorderQuery q = sortBy (\a b -> compare (idQ a) (idQ b)) q
    idQ (Select  _) = 10 -- select as last
    idQ (Limit   _) = 9
    idQ (OrderBy _) = 8 -- orderby before select (to ensure we can order by any column)
    idQ (GroupBy _) = 7
    idQ (Where   _) = 6


------
--- SELECT
------
executeQuery
    :: ([(String, Cell)], [[[Cell]]])
    -> SubQuery
    -> Either String ([(String, Cell)], [[[Cell]]])
executeQuery (schema, rowgroups) (Select cols) = if err0 == ""
    then Right (newSchema, extractedRows)
    else Left err0
  where
    schemaCols       = map fst schema
    mbSchemaIndicies = zip cols (map (`elemIndex` schemaCols) cols)
    invalidCols      = map fst (filter (isNothing . snd) mbSchemaIndicies)
    err0             = if null invalidCols
        then ""
        else "Invalid columns " ++ intercalate "," invalidCols ++ " requested"
    schemaIndicies = map (\(_, Just x) -> x) mbSchemaIndicies
    newSchema      = map (schema !!) schemaIndicies
    extractedRows  = map rowMapper rowgroups
    rowMapper rows = map (\x -> map (x !!) schemaIndicies) rows

------
--- LIMIT
------
executeQuery (schema, rowgroups) (Limit rowCount) =
    Right (schema, [take (fromInteger rowCount) (concat rowgroups)])

------
--- ORDER BY
------

executeQuery (schema, rowgroups) (OrderBy (order, columns)) =
    case sortedRows of
        (Right res) -> Right (schema, [res])
        (Left  err) -> Left err

  where
    comparer = case order of
        Asc  -> cellCompareAsc
        Desc -> cellCompareDesc
    schemaCols = map fst schema
    rows       = concat rowgroups
    sortedRows = foldl folder (Right rows) columns
    folder (Right cells) column = orderByOne schemaCols column cells comparer
    folder (Left  msg  ) column = Left msg

------
--- WHERE
------
executeQuery (schema, rowgroups) (Where expr) = case result of
    Left  s            -> Left s
    Right filteredRows -> Right (schema, [filteredRows])

  where
    rows       = concat rowgroups
    resultRows = map
        (\r ->
            (evalExpr schema r (Operation expr boolAnd (Const (CBool True))), r)
        )
        rows
    tmp = foldl (\acc (e, _) -> acc >> e)
                (Right (CBool True) :: Either String Cell)
                resultRows
    result = case tmp of
        Left  err -> Left err
        Right _   -> Right
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

executeQuery t@(schema, rowgroups) (GroupBy columns) = case groupedRows of
    Left  rg  -> Right (schema, [map head rg])
    Right err -> Left err
    where groupedRows = createGroupsBy t columns


createGroupsBy :: Table -> [String] -> Either [[[Cell]]] [Char]
createGroupsBy (schema, rowGroups) columns = if null err0
    then Left groupedRows
    else Right err0
  where
    schemaCols       = map fst schema
    mbSchemaIndicies = zip columns (map (`elemIndex` schemaCols) columns)
    invalidCols      = map fst (filter (isNothing . snd) mbSchemaIndicies)
    err0             = if null invalidCols
        then ""
        else "Invalid columns " ++ intercalate "," invalidCols ++ " requested"
    colIdcs     = map (fromJust . snd) mbSchemaIndicies
    rows        = concat rowGroups
    groupedRows = Data.List.groupBy (sameOnIdcs colIdcs) rows
    sameOnIdcs idcs ra rb = all (\i -> (ra !! i) == (rb !! i)) idcs

------
--- EXECUTOR HELPERS
------

orderByOne
    :: [String]
    -> String
    -> [[Cell]]
    -> (Cell -> Cell -> Either String Ordering)
    -> Either String [[Cell]]
orderByOne schema column rows comparer = if isNothing mbColIdx
    then Left $ "Could not find column " ++ column
    else Right sortedRows
  where
    sortedRows = Data.List.sortBy
        (\r1 r2 -> case comparer (r1 !! colIdx) (r2 !! colIdx) of
            Left s -> error
                ("Table parsed into invalid format! Attempted to compare incompatible types: "
                ++ s
                )
            Right ord -> ord
        )
        rows
    mbColIdx = elemIndex column schema
    colIdx   = fromJust mbColIdx