module Operations
    ( prettyPrintTable
    , parseCsv
    , parseQuery
    , execute
    ) where
import           Data.Char                      ( isSpace )
import           Data.List                      ( elemIndex
                                                , intercalate
                                                , intersperse
                                                , nub
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

padTo :: Int -> Char -> String -> String
padTo padding what string =
    " " ++ string ++ replicate (padding - length string) what ++ " "


prettyPrintSchema :: [(String, Cell)] -> [Int] -> IO ()
prettyPrintSchema schema padding = putStrLn
    $ intercalate "|" (zipWith (\s p -> padTo p ' ' (fst s)) schema padding)

prettyPrintSingleGroup :: [Row] -> [Int] -> IO ()
prettyPrintSingleGroup rows padding = mapM_ (prettyPrintRow padding) rows

prettyPrintRow :: [Int] -> Row -> IO ()
prettyPrintRow padding row = putStrLn $ intercalate
    "|"
    (zipWith (\cell pad -> padTo pad ' ' (prettyPrintCell cell)) row padding)

-----------------
---TABLES PARSING
-----------------
type Delimiter = String
type SchemaDescription = String
parseCsv :: Delimiter -> SchemaDescription -> Handle -> IO Table
parseCsv delimiter schemaDesc handle = do
    contents <- hGetContents handle
    let rawRows   = lines contents
    let splitRows = map (splitOn delimiter) rawRows
    let header = if null rawRows
            then error "No rows, schema could not be loaded!"
            else head splitRows
    let body            = tail splitRows
    let schema          = createDefualtSchema header schemaDesc
    let schemaCellOrder = map snd schema
    let tbody           = map (parseWithSchema schemaCellOrder) body
    return (schema, [tbody])


createDefualtSchema :: [String] -> String -> [(String, Cell)]
createDefualtSchema names repr = if valid
    then zipWith createSchemaCell names normalizedRepr
    else error "Invalid schema specification/column names"
  where
    normalizedRepr = map Char.toLower repr
    valid =
        all (\c -> c == 'i' || c == 'd' || c == 'b' || c == 's') normalizedRepr
            && nub names
            == names
            && length names
            == length repr
    createSchemaCell name c = case c of
        'i' -> (name, CInt 0)
        's' -> (name, CStr "")
        'd' -> (name, CDouble 0)
        'b' -> (name, CBool False)
        _   -> error "Invalid specification"

-- parses a row according to a schema (order of Cells in a list)
-- errors out on invalid (unconvertible) values
-- SCHEMA -> STRING VALUES OF A ROW -> PARSED ROW
parseWithSchema :: [Cell] -> [[Char]] -> [Cell]
parseWithSchema = zipWith parser
  where
    parser cellTemplate cellValue = unwrapOrErr
        (  "Invalid value "
        ++ cellValue
        ++ " for the schema type "
        ++ typeOfCell cellTemplate
        )
        (parseCell cellTemplate cellValue)

-------
--- QUERY PARSING
-------

parseSequenceWithAccumulation :: [Parser SubQuery] -> Parser [SubQuery]
parseSequenceWithAccumulation parsers@(p : ps) =
    p
        >>= (\result ->
                whitespace
                    >>  parseSequenceWithAccumulation ps
                    >>= (\sndRes -> parserPure (result : sndRes))
            )
parseSequenceWithAccumulation [] = parserPure []

clauses = ["select", "where", "groupby", "orderby", "limit"]

parseQuery :: String -> Maybe [SubQuery]
parseQuery q = case res of
    (Just (result, rest)) -> if null rest then Just result else Nothing
    Nothing               -> Nothing
  where
    res = runParser (p >>= (\res -> whitespace >> parserPure res)) q
    p   = parseSequenceWithAccumulation [parseSelect, parseLimit]
        `orElse` parseSequenceWithAccumulation [parseSelect]
parseSelect =
    Parser.lowerUpperString "SELECT "
        >>  whitespace
        >>  colListParser
        >>= (parserPure . Select)
parseLimit =
    Parser.lowerUpperString "LIMIT "
        >>  whitespace
        >>  positiveInteger
        >>= (parserPure . Limit . read)

positiveInteger = some (Parser.satisfy (\c -> c `elem` ['0' .. '9']))

-- word(,word)*
colListParser =
    word
        >>= (\matchedWord ->
                many spacedWord
                    >>= (\matchedWords ->
                            parserPure (matchedWord : matchedWords)
                        )
            )
--[ws],[ws]word
spacedWord = Parser.whitespace >> string "," >> Parser.whitespace >> word
-- word OR `word`
word =
    notClause
        `orElse` (   Parser.string "`"
                 >>  pureWord
                 >>= (\match -> Parser.string "`" >> parserPure match)
                 )
pureWord = Parser.some
    (Parser.satisfy (\c -> not $ isSpace c || (c == '`') || (c == ',')))
notClause =
    pureWord
        >>= (\match -> if map Char.toLower match `elem` clauses
                then Parser.failure
                else parserPure match
            )


execute query table = foldl exec table reorderedQuery
  where
    reorderedQuery = reorderQuery query
    exec (Left  table) qry = executeQuery table qry
    exec (Right error) _   = Right error
    reorderQuery q = sortWith (\a b -> idQ a < idQ b) q
    idQ (Select  _) = 10 -- select as last
    idQ (Limit   _) = 9
    idQ (OrderBy _) = 8 -- orderby before select (to ensure we can order by any column)
    idQ (GroupBy _) = 7
    idQ (Where   _) = 6

type QueryResult = Either Table String


------
--- SELECT
------
executeQuery (schema, rowgroups) (Select cols) = if err0 == ""
    then Left (newSchema, extractedRows)
    else Right err0
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
    Left (schema, [take (fromInteger rowCount) (concat rowgroups)])

------
--- ORDER BY
------

executeQuery (schema, rowgroups) (OrderBy (order, columns)) =
    case sortedRows of
        (Left  res) -> Left (schema, [res])
        (Right err) -> Right err

  where
    comparer = case order of
        Asc  -> (>)
        Desc -> (<)
    schemaCols = map fst schema
    rows       = concat rowgroups
    sortedRows = foldl folder (Left rows) columns
    folder (Left  cells) column = orderByOne schemaCols column cells comparer
    folder (Right msg  ) column = Right msg

orderByOne
    :: [String]
    -> String
    -> [[Cell]]
    -> (Cell -> Cell -> Bool)
    -> Either [[Cell]] String
orderByOne schema column rows comparer = if isNothing mbColIdx
    then Right $ "Could not found column " ++ column
    else Left sortedRows
  where
    sortedRows =
        sortWith (\r1 r2 -> comparer (r1 !! colIdx) (r2 !! colIdx)) rows
    mbColIdx = elemIndex column schema
    colIdx   = fromJust mbColIdx


mergeWith :: (a -> a -> Bool) -> [a] -> [a] -> [a]
mergeWith _    []        s         = s
mergeWith _    f         []        = f
mergeWith comp (fa : fb) (sa : sb) = if comp fa sa
    then fa : mergeWith comp fb (sa : sb)
    else sa : mergeWith comp (fa : fb) sb

partition :: [a] -> ([a], [a])
partition a = partition' a a

partition' :: [a] -> [a] -> ([a], [a])
partition' (a : b) (_ : _ : z) =
    let (left, right) = partition' b z in (a : left, right)
partition' a b = ([], a)

sortWith :: (a -> a -> Bool) -> [a] -> [a]
sortWith _ [a] = [a]
sortWith _ []  = []
sortWith cmp a =
    let (left, right) = partition a
        lSorted       = sortWith cmp left
        rSorted       = sortWith cmp right
    in  mergeWith cmp lSorted rSorted

