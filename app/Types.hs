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
  | Where Expr
  | GroupBy [String]
  | OrderBy (Order, [String])
  | Limit Integer

instance Eq SubQuery where
    Select  x         == Select  y         = x == y
    GroupBy x         == GroupBy y         = x == y
    OrderBy (Asc , x) == OrderBy (Asc , y) = x == y
    OrderBy (Desc, x) == OrderBy (Desc, y) = x == y
    Where   x         == Where   y         = x == y
    Limit   x         == Limit   y         = x == y
    _                 == _                 = False

instance Show SubQuery where
    show (Select  x        ) = "Select(" ++ show x ++ ")"
    show (GroupBy x        ) = "GroupBy(" ++ show x ++ ")"
    show (Limit   x        ) = "Limit(" ++ show x ++ ")"
    show (OrderBy (Asc , x)) = "OrderBy( ASC, " ++ show x ++ ")"
    show (OrderBy (Desc, x)) = "OrderBy( ASC, " ++ show x ++ ")"
    show (Where   e        ) = "Where(" ++ show e ++ ")"

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
queryCheckOrderBy ((OrderBy (_, cols)) : rest)
    | null cols = (False, "Empty OrderBy")
    | otherwise = queryCheckLimit rest
queryCheckOrderBy list = propagateIfErr $ queryCheckLimit list

queryCheckGroupBy :: Query -> (Bool, String)
queryCheckGroupBy [] = (True, "")
queryCheckGroupBy ((GroupBy cols) : rest) | null cols = (False, "Empty GroupBy")
                                          | otherwise = queryCheckOrderBy rest
queryCheckGroupBy list = propagateIfErr $ queryCheckOrderBy list

queryCheckWhere :: Query -> (Bool, String)
queryCheckWhere []                 = (True, "")
queryCheckWhere ((Where _) : list) = propagateIfErr $ queryCheckGroupBy list
queryCheckWhere list               = propagateIfErr $ queryCheckGroupBy list

queryCheck :: Query -> (Bool, String)
queryCheck [] = (False, "Empty query")
queryCheck ((Select cols) : rest) | null cols = (False, "Empty select")
                                  | otherwise = queryCheckWhere rest
queryCheck _ = (False, "Query must begin with a Select subquery")


--- PARSING HELPERS

data Expr
    = Col String
    | Const Cell
    | Operation {left :: Expr, op :: Cell -> Cell -> Either String Cell, right :: Expr}

instance Show Expr where
    show (Col   c        ) = "ColExpr(" ++ show c ++ ")"
    show (Const c        ) = "ConsExpr(" ++ show c ++ ")"
    show (Operation l _ r) = "(" ++ show l ++ "?" ++ show r ++ ")"

instance Eq Expr where
    (Col   c        ) == (Col   d          ) = c == d
    (Const c        ) == (Const d          ) = c == d
    (Operation l _ r) == (Operation l1 _ r1) = l == l1 && r == r1
    _                 == _                   = False

ensureAndContinue
    :: String
    -> (Cell -> Cell -> Bool)
    -> (Cell -> Cell -> a)
    -> Cell
    -> Cell
    -> Either String a
ensureAndContinue desc judge finalFn argl argr
    | judge argl argr = Right (finalFn argl argr)
    | otherwise = Left
        (  "Incompatible types for operation `"
        ++ desc
        ++ "` detected ("
        ++ showCellsWith typeOfCell [argl, argr]
        ++ ")"
        )


-- TODO: unary operator support (not, unary -)
boolCheck (CBool _) (CBool _) = True
boolCheck _         _         = False

add = ensureAndContinue "+" compatibleArith (+)
mul = ensureAndContinue "*" compatibleArith (*)
sub = ensureAndContinue "-" compatibleArith (-)
div = ensureAndContinue "/" compatibleArith (/)
boolAnd =
    ensureAndContinue "&" boolCheck (\(CBool a) (CBool b) -> CBool (a && b))
boolOr =
    ensureAndContinue "|" boolCheck (\(CBool a) (CBool b) -> CBool (a || b))
boolXor = ensureAndContinue
    "^"
    boolCheck
    (\(CBool a) (CBool b) -> CBool ((a || b) && (not a || not b)))
cellCompareAsc = ensureAndContinue
    "comparison"
    compatibleOrd
    (\a b -> if a < b then LT else if a == b then EQ else GT)
cellCompareDesc = ensureAndContinue
    "comparison"
    compatibleOrd
    (\a b -> if a < b then GT else if a == b then EQ else LT)

compatibleOrdEq x y = compatibleEq x y && compatibleEq x y

cellLeq = ensureAndContinue "<=" compatibleOrdEq (\a b -> CBool (a <= b))
cellLe = ensureAndContinue "<" compatibleOrd (\a b -> CBool (a < b))
cellGeq = ensureAndContinue ">=" compatibleOrdEq (\a b -> CBool (a >= b))
cellGe = ensureAndContinue ">" compatibleOrd (\a b -> CBool (a > b))
cellEq = ensureAndContinue "== (equal)" compatibleEq (\a b -> CBool (a == b))
cellNeq =
    ensureAndContinue "!= (not equal)" compatibleEq (\a b -> CBool (a /= b))

unwrapMaybe _      (Just val) = val
unwrapMaybe errMsg _          = error errMsg

getIdxOrErr errMsg row index | length row <= index = error errMsg
                             | otherwise           = row !! index

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
    foundItem = lookup colName schema
    found     = isJust foundItem
    unwrapped = unwrapMaybe "impossible error" foundItem
    index =
        unwrapMaybe "impossible error" (elemIndex (colName, unwrapped) schema)
    item        = getIdxOrErr "Row does not adhere to schema" row index
    areSameType = typeOfCell item == typeOfCell unwrapped

evalExpr schema row (Operation l op r) =
    evalExpr schema row l >>= (\lres -> evalExpr schema row r >>= op lres)
