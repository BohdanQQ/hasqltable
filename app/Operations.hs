module Operations
    ( prettyPrintTable
    , parseCsv
    , parseQuery
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

-- accumulates the results of parsers into an array
-- all parsers must pass in order for this parser to produce a value
parseSequenceWithAccumulation :: [Parser (Maybe a)] -> Parser [a]
parseSequenceWithAccumulation parsers@(p : ps) =
    p
        >>= (\result ->
                whitespace
                    >>  parseSequenceWithAccumulation ps
                    >>= (\sndRes -> parserPure (extract result ++ sndRes))
            )
  where
    extract (Just x) = [x]
    extract _        = []
parseSequenceWithAccumulation [] = parserPure []

-- the lowercase variants of the clauses used in the query syntax
clauses :: [[Char]]
clauses = ["select", "where", "groupby", "orderby", "limit"]

parseQuery :: String -> Maybe [SubQuery]
parseQuery q = case res of
    (Just (result, rest)) -> if null rest then Just result else Nothing
    Nothing               -> Nothing
  where
    res = runParser (p >>= (\res -> whitespace >> parserPure res)) q
    p   = parseSequenceWithAccumulation
        (wrapWithJust parseSelect : safeSubqueries)
    notMandCombSubqueries = [parseSimpleWhere, parseGroupBy, parseOrderBy, parseLimit]
    safeSubqueries        = map safeSubquerryMapper notMandCombSubqueries
    safeSubquerryMapper p = wrapWithJust p `orElse` parserPure Nothing

--subquery parsers

parseSelect :: Parser SubQuery
parseSelect =
    lowerUpperString "SELECT "
        >>  whitespace
        >>  colListParser
        >>= (parserPure . Select)

parseLimit :: Parser SubQuery
parseLimit =
    lowerUpperString "LIMIT "
        >>  whitespace
        >>  positiveInteger
        >>= (parserPure . Limit . read)

parseSimpleWhere :: Parser SubQuery
parseSimpleWhere =
    lowerUpperString "WHERE "
        >>  whitespace
        >>  simpleExpr
        >>= (\e -> whitespace >> parserPure (Where e))

parseOrderBy :: Parser SubQuery
parseOrderBy =
    lowerUpperString "ORDERBY "
        >>  whitespace
        >>  parseSequenceWithAccumulation
                [wrapWithJust orderByOrderWs, wrapWithJust colListParser]
        >>= (\[[order], columns] ->
                whitespace >> parserPure (OrderBy (getOrder order, columns))
            )

parseGroupBy :: Parser SubQuery
parseGroupBy =
    lowerUpperString "GROUPBY "
        >>  whitespace
        >>  colListParser
        >>= (\columns -> whitespace >> parserPure (GroupBy columns))

wrapWithJust :: Parser a -> Parser (Maybe a)
wrapWithJust p = p >>= (parserPure . Just)
getOrder :: [Char] -> Order
getOrder o = if map toLower o == "asc" then Asc else Desc

orderByOrderWs :: Parser [[Char]]
orderByOrderWs =
    (lowerUpperString "asc" `orElse` lowerUpperString "desc")
        >>= (\ord -> whitespace >> parserPure [ord])

boolExpr :: Parser Expr
boolExpr =
    (Parser.lowerUpperString "true" >> parserPure (Const (CBool True)))
        `orElse` (  Parser.lowerUpperString "false"
                 >> parserPure (Const (CBool False))
                 )

-- parses terminal expression of the where clause expression
termExpr :: Parser Expr
termExpr =
    (escapedWord >>= (parserPure . Col))
        `orElse` boolExpr
        `orElse` (double >>= (parserPure . Const . CDouble . read))
        `orElse` (integer >>= (parserPure . Const . CInt . read))
        `orElse` (stringLiteral >>= (parserPure . Const . CStr))
        -- TODO operation literals    

-- parses (leftTerminalExpr op rightTerminalExpr)-type expression 
opExpr :: Parser Expr
opExpr =
    termExpr
        >>= (\lexpr ->
                whitespace
                    >>  arithOperation
                    >>= (\op ->
                            whitespace
                                >>  termExpr
                                >>= (parserPure . Operation lexpr op)
                        )
            )

-- parses one side of the where clause expression
sideExpr :: Parser Expr
sideExpr = opExpr `orElse` termExpr

-- parses the entire where clause expression
-- (a op b) (boolOp) (c op d)
-- a (boolOp) b
-- boolExpr
-- or a combination
simpleExpr :: Parser Expr
simpleExpr =
    -- (a+b) == "c"
    (   sideExpr
        >>= (\lexpr ->
                whitespace
                    >>  whereOperation
                    >>= (\op ->
                            whitespace
                                >>  sideExpr
                                >>= (parserPure . Operation lexpr op)
                        )
            )
        )
        `orElse` boolExpr

------
--- EXPRESSION OPERATION PARSERS
------
arithOperation :: Parser (Cell -> Cell -> Cell)
arithOperation =
    (string "+" >> parserPure add)
        `orElse` (string "-" >> parserPure sub)
        `orElse` (string "*" >> parserPure mul)
        `orElse` (string "/" >> parserPure Types.div)

whereOperation :: Parser (Cell -> Cell -> Cell)
whereOperation =
    (string "&" >> parserPure boolAnd)
        `orElse` (string "|" >> parserPure boolOr)
        `orElse` (string "^" >> parserPure boolXor)
        `orElse` (string "<=" >> parserPure (\a b -> CBool (a <= b)))
        `orElse` (string "<" >> parserPure (\a b -> CBool (a < b)))
        `orElse` (string ">=" >> parserPure (\a b -> CBool (a >= b)))
        `orElse` (string ">" >> parserPure (\a b -> CBool (a > b)))
        `orElse` (string "==" >> parserPure (\a b -> CBool (a == b)))

------
--- HELPER PARSERS
------
stringLiteral :: Parser [Char]
stringLiteral =
    string "\""
        >>  some (satisfy (\x -> x `notElem` ['"', '\n']))
        >>= (\res -> string "\"" >> parserPure res)

double :: Parser [Char]
double =
    (        parseSequenceWithAccumulation
                [ wrapWithJust integer
                , wrapWithJust (string ".")
                , wrapWithJust positiveInteger
                ]
        `orElse` parseSequenceWithAccumulation [wrapWithJust integer]
        )
        >>= (parserPure . concat)

integer :: Parser [Char]
integer = negativeInteger `orElse` positiveInteger
positiveInteger :: Parser [Char]
positiveInteger = some (Parser.satisfy (\c -> c `elem` ['0' .. '9']))
negativeInteger :: Parser [Char]
negativeInteger =
    string "-"
        >>  whitespace
        >>  positiveInteger
        >>= (\res -> parserPure ('-' : res))

-- parses the select clause column list 
-- word(,word)*
colListParser :: Parser [[Char]]
colListParser =
    word
        >>= (\matchedWord ->
                many spacedWord
                    >>= (\matchedWords ->
                            parserPure (matchedWord : matchedWords)
                        )
            )
--[whistepace],[whitespace]word
spacedWord :: Parser [Char]
spacedWord = Parser.whitespace >> string "," >> Parser.whitespace >> word

-- word OR `word`
word :: Parser [Char]
word = notClause `orElse` escapedWord

--`word`
escapedWord :: Parser [Char]
escapedWord =
    Parser.string "`"
        >>  pureWord
        >>= (\match -> Parser.string "`" >> parserPure match)

-- ensures the parsed word is not a form of any clause
notClause :: Parser [Char]
notClause =
    pureWord
        >>= (\match -> if map Char.toLower match `elem` clauses
                then Parser.failure
                else parserPure match
            )

pureWord :: Parser [Char]
pureWord = Parser.some
    (Parser.satisfy (\c -> not $ isSpace c || (c == '`') || (c == ',')))

-- executes a query on a table
execute
    :: [SubQuery]
    -> Either ([(String, Cell)], [[[Cell]]]) [Char]
    -> Either ([(String, Cell)], [[[Cell]]]) [Char]
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
--- MAIN QUERY EXECUTOR
------
------
--- SELECT
------
executeQuery
    :: ([(String, Cell)], [[[Cell]]])
    -> SubQuery
    -> Either ([(String, Cell)], [[[Cell]]]) [Char]
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
        Asc  -> (\a b -> if a < b then LT else if a == b then EQ else GT)
        Desc -> (\a b -> if a < b then GT else if a == b then EQ else LT)
    schemaCols = map fst schema
    rows       = concat rowgroups
    sortedRows = foldl folder (Left rows) columns
    folder (Left  cells) column = orderByOne schemaCols column cells comparer
    folder (Right msg  ) column = Right msg

------
--- WHERE
------
executeQuery (schema, rowgroups) (Where expr) = Left (schema, [filteredRows])
  where
    rows       = concat rowgroups
    resultRows = map
        (\r ->
            (evalExpr schema r (Operation expr boolAnd (Const (CBool True))), r)
        )
        rows
    filteredRows = map snd (filter (okResult . fst) resultRows)
    okResult res = case res of
        (CBool x) -> x
        _         -> False

------
--- GROUP_BY
------

executeQuery t@(schema, rowgroups) (GroupBy columns) = case groupedRows of
    Left  rg  -> Left (schema, [map head rg])
    Right err -> Right err
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
    -> (Cell -> Cell -> Ordering)
    -> Either [[Cell]] String
orderByOne schema column rows comparer = if isNothing mbColIdx
    then Right $ "Could not found column " ++ column
    else Left sortedRows
  where
    sortedRows =
        Data.List.sortBy (\r1 r2 -> comparer (r1 !! colIdx) (r2 !! colIdx)) rows
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

