module Parsing
    ( parseCsv
    , parseQuery
    , parseCell
    ) where
import           Data.Char                      ( isSpace
                                                , toLower
                                                )
import           Data.List                      ( nub )
import           Data.List.NonEmpty             ( sortWith )
import           Data.List.Split                ( splitOn )
import           GHC.IO.Handle                  ( Handle
                                                , hGetContents
                                                )
import           Parser
import           Types
import Text.Read
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


parseCell :: Cell -> String -> Maybe Cell
parseCell (CStr    _) s = Just (CStr s)
parseCell (CInt    _) s = let i = readMaybe s in unwrapWith CInt i
parseCell (CDouble _) s = let i = readMaybe s in unwrapWith CDouble i
parseCell (CBool   _) s = let i = readMaybe s in unwrapWith CBool i

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
    notMandCombSubqueries =
        [parseSimpleWhere, parseGroupBy, parseOrderBy, parseLimit]
    safeSubqueries = map safeSubquerryMapper notMandCombSubqueries
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
arithOperation :: Parser (Cell -> Cell -> Either String Cell)
arithOperation =
    (string "+" >> parserPure add)
        `orElse` (string "-" >> parserPure sub)
        `orElse` (string "*" >> parserPure mul)
        `orElse` (string "/" >> parserPure Types.div)

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
        >>= (\match -> if map toLower match `elem` clauses
                then Parser.failure
                else parserPure match
            )

pureWord :: Parser [Char]
pureWord = Parser.some
    (Parser.satisfy (\c -> not $ isSpace c || (c == '`') || (c == ',')))
