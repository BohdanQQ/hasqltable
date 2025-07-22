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
import Control.Applicative (liftA3)
import Data.Functor ((<&>), ($>))
import Data.Maybe (maybeToList)
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
        splitRows = map (splitOn delimiter) rawRows
        header = if null rawRows
            then error "No rows, schema could not be loaded!"
            else head splitRows
        body            = tail splitRows
        schema          = createDefualtSchema header schemaDesc
        schemaCellOrder = map snd schema
        tbody           = map (parseWithSchema schemaCellOrder) body
    return (schema, [tbody])


-- | creates schema representation using a list of column names
-- and a schema description (string of i d b s characters) 
createDefualtSchema :: [String] -> SchemaDescription -> [(String, SchemaType)]
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
        'i' -> (name, SInt)
        's' -> (name, SStr)
        'd' -> (name, SDouble)
        'b' -> (name, SBool)
        _   -> error $ "Invalid schema specification \"" ++ show c ++ "\" for column" ++ name

-- | parses a row according to a schema (order of Cells in a list)
--
-- errors out on invalid (unconvertible) values
--
-- SCHEMA -> STRING VALUES OF A ROW -> PARSED ROW
parseWithSchema :: [SchemaType] -> [[Char]] -> [Cell]
parseWithSchema = zipWith parser
  where
    unwrapOrErr :: [Char] -> Maybe p -> p
    unwrapOrErr message Nothing  = error message
    unwrapOrErr _       (Just a) = a
    parser cellTemplate cellValue = unwrapOrErr
        (  "Invalid value "
        ++ cellValue
        ++ " for the schema type "
        ++ typeOfSchemaStr cellTemplate
        )
        (parseCell cellTemplate cellValue)


parseCell :: SchemaType -> String -> Maybe Cell
parseCell SStr    s = Just $ CStr s
parseCell SInt    s = readMaybe s <&> CInt
parseCell SDouble s = readMaybe s <&> CDouble
parseCell SBool   s = readMaybe s <&> CBool

-------
--- QUERY PARSING
-------

-- | accumulates the results of parsers into an array
-- all parsers must pass in order for this parser to produce a list
--
-- Just variants are unwrapped into the list and Nothing variants are ignored
parseSequenceWithAccumulation :: [Parser (Maybe a)] -> Parser [a]
parseSequenceWithAccumulation (p : ps) =
    liftA2 (\x y -> maybeToList x ++ y) p (whitespace *> parseSequenceWithAccumulation ps)
parseSequenceWithAccumulation [] = pure []

-- | Wraps a successful parser result in a Maybe
wrapWithJust :: Parser a -> Parser (Maybe a)
wrapWithJust p = Just <$> p

-- | Parses a query string into a list of subqueries in the order as they appear in the input string
parseQuery :: String -> Maybe [SubQuery]
parseQuery q = do
    (result, rest) <- runParser (p <* whitespace) q
    if null rest then Just result else Nothing
  where
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
        wrapWithJust parser `orElse` pure Nothing

--------------------------------------------------------------------------------
--- subquery parsers
--------------------------------------------------------------------------------

-- | parses the entire SELECT clause in the form "SELECT [nColumn | \`column\`]"
-- where nColumn is a string not equal to any of the clauses (select, ...)
-- the \`column`\ option is there to provide a way to refer to such columns
parseSelect :: Parser SubQuery
parseSelect =
    Select <$> (lowerUpperString "SELECT "
        >>  whitespace >> colListParser)

-- | parses the entire LIMIT clause in the form "LIMIT positiveInteger"
parseLimit :: Parser SubQuery
parseLimit =
    Limit . read <$> (lowerUpperString "LIMIT "
        >>  whitespace >> positiveInteger)

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
    Where <$> (lowerUpperString "WHERE "
        >> wspaced simpleExpr)

-- | parses the entire ORDERBY clause in the form "ORDERBY [asc|desc] [nColumn | \`column\`]"
-- where nColumn is a string not equal to any of the clauses (select, ...)
-- the \`column`\ option is there to provide a way to refer to such columns
parseOrderBy :: Parser SubQuery
parseOrderBy =
    OrderBy <$> (lowerUpperString "ORDERBY "
        >>  wspaced (liftA2 (,) ascDescParser colListParser))
  where
    ascDescParser :: Parser Order
    ascDescParser =
        (lowerUpperString "asc" >> wspaceAndOrd Asc)
        `orElse` (lowerUpperString "desc" >> wspaceAndOrd Desc)
    wspaceAndOrd o = whitespace $> o

-- | parses the entire GROUPBY clause in the form "GROUPBY [nColumn | \`column\`]"
-- where nColumn is a string not equal to any of the clauses (select, ...)
-- the \`column`\ option is there to provide a way to refer to such columns
parseGroupBy :: Parser SubQuery
parseGroupBy =
    GroupBy <$> (lowerUpperString "GROUPBY "
        >>  wspaced colListParser)

-- | parses "true" or "false" (case-totally-insensitive)
boolExpr :: Parser Expr
boolExpr =
    (lowerUpperString "true" $> Const (CBool True)) `orElse` (lowerUpperString "false" $> Const (CBool False))

-- | parses terminal expression of the where clause expression
-- this is either a constant (True, False, 123, ...) or a column representation (name, age ,...)
termExpr :: Parser Expr
termExpr =
    (escapedWord <&> Col)
        `orElse` boolExpr
        `orElse` (double <&> Const . CDouble . read)
        `orElse` (integer <&> Const . CInt . read)
        `orElse` (stringLiteral <&> Const . CStr)

-- | parses (leftTerminalExpr op rightTerminalExpr)-type expression 
opExpr :: Parser Expr
opExpr =
    Operation <$> wspaced termExpr <*> wspaced arithOperation <*> termExpr

-- | parses one side of the where clause expression (on a side of a boolean operator)
sideExpr :: Parser Expr
sideExpr = opExpr `orElse` termExpr

-- | parses WHERE clause expression
--
-- for more, see parseSimpleWhere
simpleExpr :: Parser Expr
simpleExpr =
    (Operation <$> wspaced sideExpr <*> wspaced whereOperation <*> sideExpr)
    `orElse` boolExpr

-- | parses an arithmetic operator
arithOperation :: Parser (Cell -> Cell -> Either String Cell)
arithOperation =
    (string "+" $> add)
        `orElse` (string "-" $> sub)
        `orElse` (string "*" $> mul)
        `orElse` (string "/" $> Types.div)

-- | parses a where clause boolean operator
whereOperation :: Parser (Cell -> Cell -> Either String Cell)
whereOperation =
    (string "&" $> boolAnd)
        `orElse` (string "|"  $> boolOr)
        `orElse` (string "^"  $> boolXor)
        `orElse` (string "<=" $> cellLeq)
        `orElse` (string "<"  $> cellLe)
        `orElse` (string ">=" $> cellGeq)
        `orElse` (string ">"  $> cellGe)
        `orElse` (string "==" $> cellEq)
        `orElse` (string "!=" $> cellNeq)

--------------------------------------------------------------------------------
--- helpers
--------------------------------------------------------------------------------
-- | matches a string literal "str", str excludes the " and newline characters
stringLiteral :: Parser [Char]
stringLiteral =
    string "\"" >>  some (satisfy (\x -> x `notElem` ['"', '\n'])) <* string "\""

-- | matches a "double" literal "dbl" in the form of a decimal number (decimal separator is a . (dot))
double :: Parser [Char]
double = decimal `orElse` integer
    where
        decimal = liftA3 (\x y z -> concat [x, y, z]) integer (string ".") positiveInteger


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
        <&> ('-' :)

-- | parses the column list [nColumn | \`column\`]"
-- where nColumn is a string not equal to any of the clauses (select, ...)
-- the \`column`\ option is there to provide a way to refer to such columns
--
-- the pattern looks like this (with whitespaces in between):
-- word(,word)*
colListParser :: Parser [[Char]]
colListParser = liftA2 (:) (whitespace >> word) (many spacedWord)
  where
    -- | matches the following pattern: (whistepace)*,(whitespace)*word
    spacedWord = whitespace >> string "," >> whitespace >> word

-- | parses a word OR \`word\`
word :: Parser [Char]
word = notClause `orElse` escapedWord

-- | parses \`word\`
escapedWord :: Parser [Char]
escapedWord = string "`" >> pureWord <* string "`"

-- | ensures the parsed word is not a form of any clause
notClause :: Parser [Char]
notClause = do
    match <- pureWord
    if map toLower match `elem` clauses
        then failure
        else pure match
    where clauses = ["select", "where", "groupby", "orderby", "limit"]

-- | simple word for a list of words (not \` nor ,)
pureWord :: Parser [Char]
pureWord = some
    (satisfy (\c -> not $ isSpace c || (c == '`') || (c == ',')))
