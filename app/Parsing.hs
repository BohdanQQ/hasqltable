module Parsing
    ( parseCsv
    , parseQuery
    , parseCell
    ) where
import           Data.Char                      ( isSpace
                                                , toLower
                                                )
import           Data.List                      ( nub )
import           Data.List.Split                ( splitOn )
import           GHC.IO.Handle                  ( Handle
                                                , hGetContents
                                                )
import           Parser
import           Text.Read
import           Types
-----------------
---TABLES PARSING
-----------------
type Delimiter = String
type SchemaDescription = String

-- | parses a csv into table according to a schema description (column types)
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


-- | creates schema representation using a list of column names
-- and a schema description (string of i d b s characters) 
createDefualtSchema :: [String] -> SchemaDescription -> [(String, Cell)]
createDefualtSchema names repr = if valid
    then zipWith createSchemaCell names normalizedRepr
    else error "Invalid schema specification/column names"
  where
    normalizedRepr = map toLower repr
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

-- | parses a row according to a schema (order of Cells in a list)
--
-- errors out on invalid (unconvertible) values
--
-- SCHEMA -> STRING VALUES OF A ROW -> PARSED ROW
parseWithSchema :: [Cell] -> [[Char]] -> [Cell]
parseWithSchema = zipWith parser
  where
    unwrapOrErr :: [Char] -> Maybe p -> p
    unwrapOrErr message Nothing  = error message
    unwrapOrErr _       (Just a) = a
    parser cellTemplate cellValue = unwrapOrErr
        (  "Invalid value "
        ++ cellValue
        ++ " for the schema type "
        ++ typeOfCell cellTemplate
        )
        (parseCell cellTemplate cellValue)


parseCell :: Cell -> String -> Maybe Cell
parseCell (CStr    _) s = Just (CStr s)
parseCell (CInt    _) s = let i = readMaybe s in i >>= Just . CInt
parseCell (CDouble _) s = let i = readMaybe s in i >>= Just . CDouble
parseCell (CBool   _) s = let i = readMaybe s in i >>= Just . CBool

-------
--- QUERY PARSING
-------

-- | accumulates the results of parsers into an array
-- all parsers must pass in order for this parser to produce a list
--
-- Just variants are unwrapped into the list and Nothing variants are ignored
parseSequenceWithAccumulation :: [Parser (Maybe a)] -> Parser [a]
parseSequenceWithAccumulation (p : ps) =
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

-- | Wraps a successful parser result in a Maybe
wrapWithJust :: Parser a -> Parser (Maybe a)
wrapWithJust p = p >>= (parserPure . Just)

-- | Parses a query string into a list of subqueries in the order as they appear in the input string
parseQuery :: String -> Maybe [SubQuery]
parseQuery q = case res of
    (Just (result, rest)) -> if null rest then Just result else Nothing
    Nothing               -> Nothing
  where
    res = runParser (p >>= (\r -> whitespace >> parserPure r)) q
    p   = parseSequenceWithAccumulation
    -- accumulates each (parsed) query 
        (wrapWithJust parseSelect : safeSubqueries)
    -- list of non-mandatory subqueries
    notMandCombSubqueries =
        [parseSimpleWhere, parseGroupBy, parseOrderBy, parseLimit]
    -- maps subqueries to safe versions (Maybe indicates Error)
    -- safeSubqueries always succeed => enables parsing subset of the list above
    -- note that parseSelect may fail and indicate the failure of the entire parsing process
    -- (i.e. SELECT clause is mandatory)
    safeSubqueries = map safeSubquerryMapper notMandCombSubqueries
    -- this parser always succeeds, but propagates errors in the form of Nothing
    -- which is then ignored by the collector (parseSequenceWithAccumulation)
    safeSubquerryMapper parser =
        wrapWithJust parser `orElse` parserPure Nothing

--------------------------------------------------------------------------------
--- subquery parsers
--------------------------------------------------------------------------------

-- | parses the entire SELECT clause in the form "SELECT [nColumn | \`column\`]"
-- where nColumn is a string not equal to any of the clauses (select, ...)
-- the \`column`\ option is there to provide a way to refer to such columns
parseSelect :: Parser SubQuery
parseSelect =
    lowerUpperString "SELECT "
        >>  whitespace
        >>  colListParser
        >>= (parserPure . Select)

-- | parses the entire LIMIT clause in the form "LIMIT positiveInteger"
parseLimit :: Parser SubQuery
parseLimit =
    lowerUpperString "LIMIT "
        >>  whitespace
        >>  positiveInteger
        >>= (parserPure . Limit . read)

-- | parses the entire WHERE clause in the form "WHERE expr"
-- where expr is either "n op n cop n op n" (op - arithmetic, cop - comparison operator)
-- 
-- or n cop n (n - \`column\` / string literal / integer literal)
-- 
-- or "bool bop bool" (bop - boolean operator)
-- 
-- or bool (bool - True/False literal or \`column\`)
parseSimpleWhere :: Parser SubQuery
parseSimpleWhere =
    lowerUpperString "WHERE "
        >>  whitespace
        >>  simpleExpr
        >>= (\e -> whitespace >> parserPure (Where e))

-- | parses the entire ORDERBY clause in the form "ORDERBY [asc|desc] [nColumn | \`column\`]"
-- where nColumn is a string not equal to any of the clauses (select, ...)
-- the \`column`\ option is there to provide a way to refer to such columns
parseOrderBy :: Parser SubQuery
parseOrderBy =
    lowerUpperString "ORDERBY "
        >>  whitespace
        >>  parseSequenceWithAccumulation
                [wrapWithJust ascDescParser, wrapWithJust colListParser]
        >>= (\[[order], columns] ->
                whitespace >> parserPure (OrderBy (getOrder order, columns))
            )
  where
    getOrder o = if map toLower o == "asc" then Asc else Desc
    -- not parsing into Order directly in order to use this parser in the parseSequenceWithAccumulation function
    ascDescParser =
        (lowerUpperString "asc" `orElse` lowerUpperString "desc")
            >>= (\ord -> whitespace >> parserPure [ord])

-- | parses the entire GROUPBY clause in the form "GROUPBY [nColumn | \`column\`]"
-- where nColumn is a string not equal to any of the clauses (select, ...)
-- the \`column`\ option is there to provide a way to refer to such columns
parseGroupBy :: Parser SubQuery
parseGroupBy =
    lowerUpperString "GROUPBY "
        >>  whitespace
        >>  colListParser
        >>= (\columns -> whitespace >> parserPure (GroupBy columns))

-- | parses "true" or "false" (case-totally-insensitive)
boolExpr :: Parser Expr
boolExpr =
    (Parser.lowerUpperString "true" >> parserPure (Const (CBool True)))
        `orElse` (  Parser.lowerUpperString "false"
                 >> parserPure (Const (CBool False))
                 )

-- | parses terminal expression of the where clause expression
-- this is either a constant (True, False, 123, ...) or a column representation (name, age ,...)
termExpr :: Parser Expr
termExpr =
    (escapedWord >>= (parserPure . Col))
        `orElse` boolExpr
        `orElse` (double >>= (parserPure . Const . CDouble . read))
        `orElse` (integer >>= (parserPure . Const . CInt . read))
        `orElse` (stringLiteral >>= (parserPure . Const . CStr))

-- | parses (leftTerminalExpr op rightTerminalExpr)-type expression 
opExpr :: Parser Expr
opExpr =
    termExpr
        >>= (\lexpr ->
                whitespace
                    >>  arithOperation
                    >>= (\o ->
                            whitespace
                                >>  termExpr
                                >>= (parserPure . Operation lexpr o)
                        )
            )

-- | parses one side of the where clause expression (on a side of a boolean operator)
sideExpr :: Parser Expr
sideExpr = opExpr `orElse` termExpr

-- | parses WHERE clause expression
--
-- for more, see parseSimpleWhere
simpleExpr :: Parser Expr
simpleExpr =
    (   sideExpr
        >>= (\lexpr ->
                whitespace
                    >>  whereOperation
                    >>= (\o ->
                            whitespace
                                >>  sideExpr
                                >>= (parserPure . Operation lexpr o)
                        )
            )
        )
        `orElse` boolExpr

-- | parses an arithmetic operator
arithOperation :: Parser (Cell -> Cell -> Either String Cell)
arithOperation =
    (string "+" >> parserPure add)
        `orElse` (string "-" >> parserPure sub)
        `orElse` (string "*" >> parserPure mul)
        `orElse` (string "/" >> parserPure Types.div)

-- | parses a where clause boolean operator
whereOperation :: Parser (Cell -> Cell -> Either String Cell)
whereOperation =
    (string "&" >> parserPure boolAnd)
        `orElse` (string "|" >> parserPure boolOr)
        `orElse` (string "^" >> parserPure boolXor)
        `orElse` (string "<=" >> parserPure cellLeq)
        `orElse` (string "<" >> parserPure cellLe)
        `orElse` (string ">=" >> parserPure cellGeq)
        `orElse` (string ">" >> parserPure cellGe)
        `orElse` (string "==" >> parserPure cellEq)
        `orElse` (string "!=" >> parserPure cellNeq)

--------------------------------------------------------------------------------
--- helpers
--------------------------------------------------------------------------------
-- | matches a string literal "str", str excludes the " and newline characters
stringLiteral :: Parser [Char]
stringLiteral =
    string "\""
        >>  some (satisfy (\x -> x `notElem` ['"', '\n']))
        >>= (\res -> string "\"" >> parserPure res)

-- | matches a "double" literal "dbl" in the form of a decimal number (decimal separator is a . (dot))
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

-- | matches a positive or negative integer
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

-- | parses the column list [nColumn | \`column\`]"
-- where nColumn is a string not equal to any of the clauses (select, ...)
-- the \`column`\ option is there to provide a way to refer to such columns
--
-- the pattern looks like this (with whitespaces in between):
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
  where
    -- | matches the following pattern: (whistepace)*,(whitespace)*word
    spacedWord = Parser.whitespace >> string "," >> Parser.whitespace >> word

-- | parses a word OR \`word\`
word :: Parser [Char]
word = notClause `orElse` escapedWord

-- | parses \`word\`
escapedWord :: Parser [Char]
escapedWord =
    Parser.string "`"
        >>  pureWord
        >>= (\match -> Parser.string "`" >> parserPure match)

-- | ensures the parsed word is not a form of any clause
notClause :: Parser [Char]
notClause =
    pureWord
        >>= (\match -> if map toLower match `elem` clauses
                then Parser.failure
                else parserPure match
            )
    where clauses = ["select", "where", "groupby", "orderby", "limit"]

-- | simple word for a list of words (not \` nor ,)
pureWord :: Parser [Char]
pureWord = Parser.some
    (Parser.satisfy (\c -> not $ isSpace c || (c == '`') || (c == ',')))
