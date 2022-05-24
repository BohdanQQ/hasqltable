module Types where
import           Data.List                      ( elemIndex
                                                , find
                                                , intercalate
                                                , intersperse
                                                , nub
                                                )
import           Data.Maybe                     ( isJust )
import           Text.Read                      ( readMaybe )

-- | The cetnral type of the program, holds a string/integer/double/bool value
-- and represents a single cell in the DB table 
data Cell
  -- | String DB Table Cell
  = CStr String
  -- | Integer DB Table Cell
  | CInt Integer
  -- | Double DB Table Cell
  | CDouble Double
  -- | Boolean DB Table Cell
  | CBool Bool

-- | defines the name and the type of a column
--
-- Cell is used to indicate the type to reduce the amount of types
-- as cells' types can be easily used to identify the column type
type SchemaItem = (String, Cell)

-- | defines: 
-- 
-- a) types of columns
--
-- b) the mapping between the name of a column and the index of a column within a row
type Schema = [SchemaItem]

-- | typeclass meant to enable runtime detection of operation compatibility
class Compat a where
    -- | indicates whether the two arguments may be compared using \< or \>
    compatibleOrd :: a -> a -> Bool
    -- | indicates whether the two arguments may be compared using ==
    compatibleEq :: a -> a -> Bool
    -- | indicates whether the two arguments may be combined using an arithmetic operator (+, -, *, /)
    compatibleArith :: a -> a -> Bool

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

-- | this type is now ignored, but a rowgroup may serve as a intermediate
-- result for aggregate operations 
--
-- the idea is that GROUPBY would create row groups (instead of only extracting first unique row)
-- and each aggregate (COUNT, SUM, ...) would operate on each group separately 
--
--
-- this way, SELECT would need to select the first row of each row group (if GROUPBY was applied)
-- and thus the aggregates would only be required to save results into the first
-- row of each group 
type RowGroup = [Row]

-- | the main type representing a table (schema + rows)
--
-- RowGroup may serve as a intermediate
-- result for aggregate operations and so far has the same meaning as a list of rows, see RowGroup for more
type Table = (Schema, [RowGroup])

-- | customizable "string::format" for a cell list
--
-- * returns: comma-separated list of showFn resutls  
showCellsWith showFn cells = intercalate "," (map showFn cells)

-- the following instances should always be used with the Comp class to avoid runtime errors
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

cellOperationError :: [Cell] -> String -> a
cellOperationError cells operationStr = error
    ("No" ++ operationStr ++ " for cell type" ++ if length cells > 1
        then "s"
        else "" ++ " " ++ showCellsWith show cells
    )

-- so far implemented only for numeric cell types
instance (Num Cell) where
    (CInt    a) + (CInt    b) = CInt (a + b)
    (CDouble a) + (CDouble b) = CDouble (a + b)
    (CInt    a) + (CDouble b) = CDouble (fromInteger a + b)
    (CDouble a) + (CInt    b) = CDouble (a + fromInteger b)
    x           + y           = cellOperationError [x, y] "plus (+)"
    (CInt    a) * (CInt    b) = CInt (a * b)
    (CDouble a) * (CDouble b) = CDouble (a * b)
    (CInt    a) * (CDouble b) = CDouble (fromInteger a * b)
    (CDouble a) * (CInt    b) = CDouble (a * fromInteger b)
    x           * y           = cellOperationError [x, y] "multiply (*)"
    abs (CInt    a) = CInt (abs a)
    abs (CDouble a) = CDouble (abs a)
    abs x           = cellOperationError [x] "abs"
    signum (CInt    a) = CInt (signum a)
    signum (CDouble a) = CDouble (signum a)
    signum x           = cellOperationError [x] "signum"
    fromInteger = CInt
    negate (CInt    a) = CInt (negate a)
    negate (CDouble a) = CDouble (negate a)
    negate x           = cellOperationError [x] "negate"

instance (Fractional Cell) where
    recip (CInt    a) = CDouble (recip (fromInteger a))
    recip (CDouble a) = CDouble (recip a)
    recip x           = cellOperationError [x] "multiplicative inverse"
    fromRational x = CDouble (fromRational x)

instance (Show Cell) where
    show (CStr    a) = 'S' : show a
    show (CInt    a) = 'I' : show a
    show (CDouble a) = 'D' : show a
    show (CBool   a) = 'B' : show a

-- | an  alternative to show, returning only the type and not the value contained in the cell
typeOfCell :: Cell -> [Char]
typeOfCell (CStr    _) = "String"
typeOfCell (CInt    _) = "Int"
typeOfCell (CDouble _) = "Double"
typeOfCell (CBool   _) = "Bool"

-- | the ORDERBY order parameter (Ascending/Descending)
data Order = Asc | Desc

-- | The building blocks of an entire query, the individual clauses 
data SubQuery
  = Select [String]
  | Where Expr
  | GroupBy [String]
  | OrderBy (Order, [String])
  | Limit Integer

type Query = [SubQuery]

-- | represents the where clause expression parts
-- 
-- the data structure supports AST, but curretnly, the WHERE clause cannot parse
-- an AST 
data Expr
    -- | column reference by name
    = Col String
    -- | constant (literals should be parsed into this variant)
    | Const Cell
    -- | an operation node (left side, operation, right side)
    | Operation {left :: Expr, op :: Cell -> Cell -> Either String Cell, right :: Expr}


-- | ensures that arguments (judged by a judge) are valid
-- and returns textual error if invalid
-- 
-- or what the finalFn applied on arguments returns  
--
-- this function is used to ensure compatibility
ensureAndExecuteOp
    :: String
    -> (Cell -> Cell -> Bool)
    -> (Cell -> Cell -> a)
    -> Cell
    -> Cell
    -> Either String a
ensureAndExecuteOp desc judge finalFn argl argr
    | judge argl argr = Right (finalFn argl argr)
    | otherwise = Left
        (  "Incompatible types for operation `"
        ++ desc
        ++ "`(for types: "
        ++ showCellsWith typeOfCell [argl, argr]
        ++ ")"
        )
-- now follows the list of operation functions used to build an expression:

-- TODO: unary operator support (not, unary -)
boolCheck (CBool _) (CBool _) = True
boolCheck _         _         = False

add = ensureAndExecuteOp "+" compatibleArith (+)
mul = ensureAndExecuteOp "*" compatibleArith (*)
sub = ensureAndExecuteOp "-" compatibleArith (-)
div = ensureAndExecuteOp "/" compatibleArith (/)
boolAnd =
    ensureAndExecuteOp "&" boolCheck (\(CBool a) (CBool b) -> CBool (a && b))
boolOr =
    ensureAndExecuteOp "|" boolCheck (\(CBool a) (CBool b) -> CBool (a || b))
boolXor = ensureAndExecuteOp
    "^"
    boolCheck
    (\(CBool a) (CBool b) -> CBool ((a || b) && (not a || not b)))
cellCompareAsc = ensureAndExecuteOp "comparison asc" compatibleOrd compare
cellCompareDesc =
    ensureAndExecuteOp "comparison desc" compatibleOrd (flip compare)

compatibleOrdEq x y = compatibleEq x y && compatibleEq x y

cellLeq = ensureAndExecuteOp "<=" compatibleOrdEq (\a b -> CBool (a <= b))
cellLe = ensureAndExecuteOp "<" compatibleOrd (\a b -> CBool (a < b))
cellGeq = ensureAndExecuteOp ">=" compatibleOrdEq (\a b -> CBool (a >= b))
cellGe = ensureAndExecuteOp ">" compatibleOrd (\a b -> CBool (a > b))
cellEq = ensureAndExecuteOp "== (equal)" compatibleEq (\a b -> CBool (a == b))
cellNeq =
    ensureAndExecuteOp "!= (not equal)" compatibleEq (\a b -> CBool (a /= b))

-- | evaluates an expression in the context of a row (adhering to a schema)
evalExpr :: Schema -> Row -> Expr -> Either String Cell
evalExpr schema row (Const cell) = Right cell
evalExpr schema row (Col colName)
    | found && areSameType = Right item
    | not found = Left ("Column " ++ colName ++ " not found in schema")
    | otherwise = Left
        (  "Type "
        ++ typeOfCell item
        ++ " should be (according to schema) "
        ++ typeOfCell unwrapped
        )
  where
    unwrapMaybe _      (Just val) = val
    unwrapMaybe errMsg _          = error errMsg

    getIdxOrErr errMsg row index | length row <= index = error errMsg
                                 | otherwise           = row !! index

    foundItem = lookup colName schema
    found     = isJust foundItem
    unwrapped = unwrapMaybe "impossible error" foundItem
    index     = unwrapMaybe "impossible error"
                            (elemIndex (colName, unwrapped) schema)
    -- it is ok to error here since the table parsing ensures the table schema is correct
    -- any discrepancy detected here means that some of the query executors must have corrupted the schema/rows
    item        = getIdxOrErr "Row does not adhere to schema" row index
    areSameType = typeOfCell item == typeOfCell unwrapped

evalExpr schema row (Operation l op r) =
    evalExpr schema row l >>= (\lres -> evalExpr schema row r >>= op lres)
