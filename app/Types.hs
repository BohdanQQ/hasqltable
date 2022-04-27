module Types where
import           Data.List                      ( elemIndex
                                                , intercalate
                                                , intersperse
                                                , nub
                                                )
import           Data.Maybe                     ( isJust )
import           Text.Read                      ( readMaybe )

data Cell
  = CStr String
  | CInt Integer
  | CDouble Double
  | CBool Bool

type SchemaItem = (String, Cell)

-- schema is not a map - must remember order
-- rewrite would cause unnecessary trouble
type Schema = [SchemaItem]

getDefaultsFromShcema :: Schema -> String -> Maybe Cell
getDefaultsFromShcema [] _ = Nothing
getDefaultsFromShcema (si@(siKey, siCellVal) : sis) key =
    if key == siKey then Just siCellVal else getDefaultsFromShcema sis key

class Compat a where
    compatibleOrd :: a -> a -> Bool -- <, >
    compatibleEq :: a -> a -> Bool -- ==
    compatibleArith :: a -> a -> Bool -- + - * /

instance (Compat Cell) where
    compatibleOrd (CInt    _) (CInt    _) = True
    compatibleOrd (CStr    _) (CStr    _) = True
    compatibleOrd (CDouble _) (CDouble _) = True
    compatibleOrd (CBool   _) (CBool   _) = True
    compatibleOrd (CInt    _) (CDouble _) = True
    compatibleOrd (CDouble _) (CInt    _) = True
    compatibleOrd _           _           = False
    compatibleEq (CInt    _) (CInt    _) = True
    compatibleEq (CStr    _) (CStr    _) = True
    compatibleEq (CDouble _) (CDouble _) = True
    compatibleEq (CBool   _) (CBool   _) = True
    compatibleEq (CInt    _) (CDouble _) = True
    compatibleEq (CDouble _) (CInt    _) = True
    compatibleEq _           _           = False
    compatibleArith (CInt    _) (CInt    _) = True
    compatibleArith (CDouble _) (CDouble _) = True
    compatibleArith (CInt    _) (CDouble _) = True
    compatibleArith (CDouble _) (CInt    _) = True
    compatibleArith _           _           = False


type Row = [Cell]

type RowGroup = [Row]

type Table = (Schema, [RowGroup])

showCellsWith showFn cells = intercalate "," (map showFn cells)

instance (Eq Cell) where
    (CStr    a) == (CStr    b) = a == b
    (CInt    a) == (CInt    b) = a == b
    (CDouble a) == (CDouble b) = a == b
    (CInt    a) == (CDouble b) = fromInteger a == b
    (CDouble a) == (CInt    b) = a == fromInteger b
    (CBool   a) == (CBool   b) = a == b
    x           == y           = error
        (  "Cannot compare incompatible cells ("
        ++ showCellsWith show [x, y]
        ++ ")"
        )

instance (Ord Cell) where
    compare (CStr    a) (CStr    b) = compare a b
    compare (CInt    a) (CInt    b) = compare a b
    compare (CDouble a) (CDouble b) = compare a b
    compare (CInt    a) (CDouble b) = compare (fromInteger a) b
    compare (CDouble a) (CInt    b) = compare a (fromInteger b)
    compare (CBool   a) (CBool   b) = compare a b
    compare x           y           = error
        (  "Cannot compare incompatible cells ("
        ++ showCellsWith show [x, y]
        ++ ")"
        )

instance (Num Cell) where
    (CInt    a) + (CInt    b) = CInt (a + b)
    (CDouble a) + (CDouble b) = CDouble (a + b)
    (CInt    a) + (CDouble b) = CDouble (fromInteger a + b)
    (CDouble a) + (CInt    b) = CDouble (a + fromInteger b)
    x + y =
        error
            (  "Cannot add incompatible cellls ("
            ++ showCellsWith show [x, y]
            ++ ")"
            )
    (CInt    a) * (CInt    b) = CInt (a * b)
    (CDouble a) * (CDouble b) = CDouble (a * b)
    (CInt    a) * (CDouble b) = CDouble (fromInteger a * b)
    (CDouble a) * (CInt    b) = CDouble (a * fromInteger b)
    x           * y           = error
        (  "Cannot multiply incompatible cellls ("
        ++ showCellsWith show [x, y]
        ++ ")"
        )
    abs (CInt    a) = CInt (abs a)
    abs (CDouble a) = CDouble (abs a)
    abs x =
        error ("No absolute value for cell type " ++ showCellsWith show [x])
    signum (CInt a) = CInt (signum a)
    signum (CDouble a) = CDouble (signum a)
    signum x = error ("No signum for cell type " ++ showCellsWith show [x])
    fromInteger = CInt
    negate (CInt a) = CInt (negate a)
    negate (CDouble a) = CDouble (negate a)
    negate x = error ("No negation for cell type " ++ showCellsWith show [x])

instance (Fractional Cell) where
    recip (CInt    a) = CDouble (recip (fromInteger a))
    recip (CDouble a) = CDouble (recip a)
    recip x           = error
        ("No multiplicative inverse for (" ++ showCellsWith show [x] ++ ")")
    fromRational x = CDouble (fromRational x)

instance (Show Cell) where
    show (CStr    a) = 'S' : show a
    show (CInt    a) = 'I' : show a
    show (CDouble a) = 'D' : show a
    show (CBool   a) = 'B' : show a

unwrapWith :: (t -> a) -> Maybe t -> Maybe a
unwrapWith what Nothing  = Nothing
unwrapWith what (Just x) = Just (what x)

unwrapOrErr :: [Char] -> Maybe p -> p
unwrapOrErr message Nothing  = error message
unwrapOrErr message (Just a) = a

parseCell :: Cell -> String -> Maybe Cell
parseCell (CStr    _) s = Just (CStr s)
parseCell (CInt    _) s = let i = readMaybe s in unwrapWith CInt i
parseCell (CDouble _) s = let i = readMaybe s in unwrapWith CDouble i
parseCell (CBool   _) s = let i = readMaybe s in unwrapWith CBool i

typeOfCell :: Cell -> [Char]
typeOfCell (CStr    _) = "String"
typeOfCell (CInt    _) = "Int"
typeOfCell (CDouble _) = "Double"
typeOfCell (CBool   _) = "Bool"

data Order = Asc | Desc

data SubQuery
  = Select [String]
  | Where ([String], [Cell] -> Bool) -- lengths of arrays are the same, cells are in the same order as the column names
  | GroupBy [String]
  | OrderBy (Order, [String])
  | Limit Integer

type Query = [SubQuery]

propagateIfErr :: (Bool, [Char]) -> (Bool, [Char])
propagateIfErr fnCallResult = if not groupRes then (False, msg) else (True, "")
    where (groupRes, msg) = fnCallResult

queryCheckLimit :: Query -> (Bool, String)
queryCheckLimit [] = (True, "")
queryCheckLimit [Limit i]
    | i < 0 = (False, "LIMIT must have a nonnegative integer as its parameter")
    | otherwise = (True, "")
queryCheckLimit (Limit _ : rest) =
    (False, "LIMIT (if present) must be the last subquery within a query")
queryCheckLimit _ =
    (False, "Invalid syntax - unexpected part of / end of query")

queryCheckOrderBy :: Query -> (Bool, String)
queryCheckOrderBy [] = (True, "")
queryCheckOrderBy ((OrderBy cols) : rest) | null cols = (False, "Empty OrderBy")
                                          | otherwise = queryCheckLimit rest
queryCheckOrderBy list = propagateIfErr $ queryCheckLimit list

queryCheckGroupBy :: Query -> (Bool, String)
queryCheckGroupBy [] = (True, "")
queryCheckGroupBy ((GroupBy cols) : rest) | null cols = (False, "Empty GroupBy")
                                          | otherwise = queryCheckOrderBy rest
queryCheckGroupBy list = propagateIfErr $ queryCheckOrderBy list

queryCheckWhere :: Query -> (Bool, String)
queryCheckWhere [] = (True, "")
queryCheckWhere ((Where (cols, func)) : rest)
    | noDuplicates = queryCheckGroupBy rest
    | otherwise = (False, "No need to have duplicate columns in where clause")
    where noDuplicates = nub cols == cols
queryCheckWhere list = propagateIfErr $ queryCheckGroupBy list

queryCheck :: Query -> (Bool, String)
queryCheck [] = (False, "Empty query")
queryCheck ((Select cols) : rest) | null cols = (False, "Empty select")
                                  | otherwise = queryCheckWhere rest
queryCheck _ = (False, "Query must begin with a Select subquery")


--- PARSING HELPERS

data Expr
    = Col String
    | Const Cell
    | Operation {left :: Expr, op :: Cell -> Cell -> Cell, right :: Expr}

ensureAndContinue
    :: String
    -> (Cell -> Cell -> Bool)
    -> (Cell -> Cell -> Cell)
    -> Cell
    -> Cell
    -> Cell
ensureAndContinue desc judge finalFn argl argr
    | judge argl argr = finalFn argl argr
    | otherwise = error
        (  "Incompatible types for operation "
        ++ desc
        ++ " detected ("
        ++ showCellsWith typeOfCell [argl, argr]
        ++ ")"
        )


-- TODO: unary operator support (not, unary -)
--       eq, neq, le, gt
boolCheck (CBool _) (CBool _) = True
boolCheck _         _         = False

add = ensureAndContinue "add" compatibleArith (+)
mul = ensureAndContinue "multiply" compatibleArith (*)
sub = ensureAndContinue "subtract" compatibleArith (-)
div = ensureAndContinue "divide" compatibleArith (/)
boolAnd =
    ensureAndContinue "and" boolCheck (\(CBool a) (CBool b) -> CBool (a && b))
boolOr =
    ensureAndContinue "or" boolCheck (\(CBool a) (CBool b) -> CBool (a || b))

getCellInSchema [] _ = Nothing
getCellInSchema ((k, c) : schema) name | name == k = Just c
                                       | otherwise = getCellInSchema schema name

unwrapMaybe _      (Just val) = val
unwrapMaybe errMsg _          = error errMsg

getIdxOrErr errMsg row index | length row <= index = error errMsg
                             | otherwise           = row !! index

evalExpr :: Schema -> Row -> Expr -> Cell
evalExpr schema row (Const cell) = cell
evalExpr schema row (Col colName)
    | found && areSameType = item
    | not found = error ("Column " ++ colName ++ " not found in schema")
    | otherwise = error
        (  "Type "
        ++ typeOfCell item
        ++ " should be (according to schema) "
        ++ typeOfCell unwrapped
        )
  where
    foundItem = getCellInSchema schema colName
    found     = isJust foundItem
    unwrapped = unwrapMaybe "impossible error" foundItem
    index =
        unwrapMaybe "impossible error" (elemIndex (colName, unwrapped) schema)
    item        = getIdxOrErr "Row does not adhere to schema" row index
    areSameType = typeOfCell item == typeOfCell unwrapped

evalExpr schema row (Operation l op r) = op lres rres
  where
    lres = evalExpr schema row l
    rres = evalExpr schema row r
