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

parseSequenceWithAccumulation :: [Parser SubQuery] -> Parser [SubQuery ]
parseSequenceWithAccumulation parsers@(p:ps) =
    p >>= (\result -> whitespace >> parseSequenceWithAccumulation ps >>= (\sndRes -> parserPure (result:sndRes)))
parseSequenceWithAccumulation [] = parserPure []

clauses = ["select", "where", "groupby", "orderby", "limit"]

parseQuery :: String -> Maybe [SubQuery]
parseQuery q = case res of
    (Just (result, rest)) -> if null rest then Just result else Nothing
    Nothing -> Nothing
  where
    res = runParser (p >>= (\res -> whitespace >> parserPure res)) q
    p =
        parseSequenceWithAccumulation [parseSelect, parseLimit]
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


execute query table = foldl exec table query
  where
    exec (Left  table) qry = executeQuery table qry
    exec (Right error) _   = Right error

type QueryResult = Either Table String


------
--- SELECT
------
executeQuery :: Table -> SubQuery -> QueryResult
executeQuery (schema, rowgroups) (Select cols) = if err0 == ""
    then Left (newSchema, extractedRows)
    else Right err0
  where
    schemaCols       = map fst schema
    mbSchemaIndicies = zip cols (map (flip elemIndex schemaCols) cols)
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

