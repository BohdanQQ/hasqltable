{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
module Types where
import           Data.List                      ( elemIndex
                                                , intercalate
                                                )
import           Data.Maybe                     ( isJust, isNothing )
import GHC.List ((!?))
import Data.Function ((&))
import Data.Bits (Bits(xor))

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

data SchemaType = SStr | SInt | SDouble | SBool

-- | defines the name and the type of a column
--
-- Cell is used to indicate the type to reduce the amount of types
-- as cells' types can be easily used to identify the column type
type SchemaItem = (String, SchemaType)

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
showCellsWith :: (a -> [Char]) -> [a] -> [Char]
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

instance (Eq SchemaType) where
    SStr    == SStr    = True
    SInt    == SInt    = True
    SDouble == SDouble = True
    SBool   == SBool   = True
    _       == _       = False

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
        else " " ++ showCellsWith show cells
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
typeOfCellStr :: Cell -> [Char]
typeOfCellStr (CStr    _) = "String"
typeOfCellStr (CInt    _) = "Int"
typeOfCellStr (CDouble _) = "Double"
typeOfCellStr (CBool   _) = "Bool"

instance Show SchemaType where
    show SStr    = "String"
    show SInt    = "Int"
    show SDouble = "Double"
    show SBool   = "Bool"

typeOfCell :: Cell -> SchemaType
typeOfCell (CStr    _) = SStr
typeOfCell (CInt    _) = SInt
typeOfCell (CDouble _) = SDouble
typeOfCell (CBool   _) = SBool

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
    | Operation {left :: Expr, operation :: Cell -> Cell -> Either String Cell, right :: Expr}


-- | ensures that arguments (judged by a judge) are valid
-- and returns textual error if invalid
-- 
-- or what the finalFn applied on arguments returns  
--
-- this function is used to ensure compatibility
ensureAndExecuteOp
    :: (Cell -> Cell -> Bool)
    -> String
    -> (Cell -> Cell -> a)
    -> Cell
    -> Cell
    -> Either String a
ensureAndExecuteOp judge desc finalFn argl argr
    | judge argl argr = Right (finalFn argl argr)
    | otherwise = Left
        (  "Incompatible types for operation `"
        ++ desc
        ++ "`(for types: "
        ++ showCellsWith typeOfCellStr [argl, argr]
        ++ ")"
        )

executeArith :: String -> (Cell -> Cell -> a) -> Cell -> Cell -> Either String a
executeArith = ensureAndExecuteOp compatibleArith

executeBool :: String -> (Cell -> Cell -> a) -> Cell -> Cell -> Either String a
executeBool = ensureAndExecuteOp boolCheck

executeOrd :: String -> (Cell -> Cell -> a) -> Cell -> Cell -> Either String a
executeOrd = ensureAndExecuteOp compatibleOrd

-- now follows the list of operation functions used to build an expression:

-- TODO: unary operator support (not, unary -)
boolCheck :: Cell -> Cell -> Bool
boolCheck (CBool _) (CBool _) = True
boolCheck _         _         = False


add :: Cell -> Cell -> Either String Cell
add = executeArith "+" (+)

mul :: Cell -> Cell -> Either String Cell
mul = executeArith "*" (*)

sub :: Cell -> Cell -> Either String Cell
sub = executeArith "-" (-)

div :: Cell -> Cell -> Either String Cell
div = executeArith "/" (/)

boolAnd :: Cell -> Cell -> Either String Cell
boolAnd =
    executeBool "&" (\(CBool a) (CBool b) -> CBool $ a && b)

boolOr :: Cell -> Cell -> Either String Cell
boolOr =
    executeBool "|" (\(CBool a) (CBool b) -> CBool $ a || b)

boolXor :: Cell -> Cell -> Either String Cell
boolXor = executeBool "^"
    (\(CBool a) (CBool b) -> CBool $ (a || b) && (not a || not b))
cellCompareAsc :: Cell -> Cell -> Either String Ordering
cellCompareAsc = executeOrd "comparison asc" compare

cellCompareDesc :: Cell -> Cell -> Either String Ordering
cellCompareDesc =
    executeOrd "comparison desc" (flip compare)

compatibleOrdEq :: Compat a => a -> a -> Bool
compatibleOrdEq x y = compatibleEq x y && compatibleEq x y

cellLeq :: Cell -> Cell -> Either String Cell
cellLeq = executeOrd "<=" (\a b -> CBool $ a <= b)

cellLe :: Cell -> Cell -> Either String Cell
cellLe = executeOrd "<" (\a b -> CBool $ a < b)

cellGeq :: Cell -> Cell -> Either String Cell
cellGeq = executeOrd ">=" (\a b -> CBool $ a >= b)

cellGe :: Cell -> Cell -> Either String Cell
cellGe = executeOrd ">" (\a b -> CBool $ a > b)

cellEq :: Cell -> Cell -> Either String Cell
cellEq = ensureAndExecuteOp compatibleEq "== (equal)" (\a b -> CBool (a == b))

cellNeq :: Cell -> Cell -> Either String Cell
cellNeq =
    ensureAndExecuteOp compatibleEq "!= (not equal)" (\a b -> CBool (a /= b))

toEither :: Monad m => (m a -> Maybe a) -> String -> m a -> Either String a
toEither extract msg x = do
    let res = extract x
    if isNothing res then Left msg else return $ unwrap res
    where
    unwrap (Just a) = a

mbToEith :: String -> Maybe a -> Either String a
mbToEith = toEither id

getCellByColname :: Schema -> Row -> String -> Either String Cell
getCellByColname schema row colName = do
    -- & mbToEither ... - error handling, feel free to stop reading at &
    colType <- lookup colName schema & mbToEith ("Column " ++ colName ++ " not found in schema")
    colIdx  <- elemIndex (colName, colType) schema & mbToEith "impossible"
    rowItemAtIdx <- row !? colIdx & mbToEith "Row does not adhere to the schema!"
    if typeOfCell rowItemAtIdx /= colType then
        Left $ "Row cell's type is "
        ++ typeOfCellStr rowItemAtIdx
        ++ " but should be (according to schema) "
        ++ show colType
    else
        return rowItemAtIdx

-- | evaluates an expression in the context of a row (adhering to a schema)
evalExpr :: Schema -> Row -> Expr -> Either String Cell
evalExpr _ _ (Const cell) = Right cell
evalExpr schema row (Col colName) = getCellByColname schema row colName
evalExpr schema row (Operation l op r) = do
    lres <- evalExpr schema row l
    rres <- evalExpr schema row r
    op lres rres
