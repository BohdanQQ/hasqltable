module Types where
import           Data.List                      ( nub, elemIndex )
import Text.Read (readMaybe)

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
    compatibleStrLowerEq :: a -> a -> Bool -- LIKE

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
    compatibleStrLowerEq (CStr _) (CStr _) = True
    compatibleStrLowerEq _        _        = False


type Row = [Cell]

type RowGroup = [Row]

type Table = (Schema, [RowGroup])

instance (Eq Cell) where
    (CStr    a) == (CStr    b) = a == b
    (CInt    a) == (CInt    b) = a == b
    (CDouble a) == (CDouble b) = a == b
    (CInt    a) == (CDouble b) = fromInteger a == b
    (CDouble a) == (CInt    b) = a == fromInteger b
    (CBool   a) == (CBool   b) = a == b
    x           == y           = error ("Cannot compare incompatible cells (" ++ show x ++ ", " ++ show y ++ ")")

instance (Ord Cell) where
    compare (CStr    a) (CStr    b) = compare a b
    compare (CInt    a) (CInt    b) = compare a b
    compare (CDouble a) (CDouble b) = compare a b
    compare (CInt    a) (CDouble b) = compare (fromInteger a) b
    compare (CDouble a) (CInt    b) = compare a (fromInteger b)
    compare (CBool   a) (CBool   b) = compare a b
    compare x           y           = error ("Cannot compare incompatible cells (" ++ show x ++ ", " ++ show y ++ ")")  

instance (Show Cell) where
    show (CStr    a) = 'S' : show a
    show (CInt    a) = 'I' : show a
    show (CDouble a) = 'D' : show a
    show (CBool   a) = 'B' : show a

unwrapWith :: (t -> a) -> Maybe t -> Maybe a
unwrapWith what Nothing = Nothing 
unwrapWith what (Just x) = Just (what x) 

unwrapOrErr :: [Char] -> Maybe p -> p
unwrapOrErr message Nothing  = error message
unwrapOrErr message (Just a) = a

parseCell :: Cell -> String -> Maybe Cell
parseCell (CStr _) s = Just (CStr s) 
parseCell (CInt _) s = let i = readMaybe s in unwrapWith CInt i 
parseCell (CDouble _) s = let i = readMaybe s in unwrapWith CDouble i 
parseCell (CBool _) s = let i = readMaybe s in unwrapWith CBool i 

typeOfCell :: Cell -> [Char]
typeOfCell (CStr _) = "String"
typeOfCell (CInt _) = "Int"
typeOfCell (CDouble _) = "Double"
typeOfCell (CBool _) = "Bool"

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

data Conditional
    = And {leftCond :: Conditional, rightCond :: Conditional}
    | Or  {leftCond :: Conditional, rightCond :: Conditional}
    | Xor  {leftCond :: Conditional, rightCond :: Conditional}
    | Not Conditional
    | NotE Expr
    | Eq  {leftExp :: Expr, rightExp :: Expr}
    | Less {leftExp :: Expr, rightExp :: Expr}
    | Greater {leftExp :: Expr, rightExp :: Expr}
    | StrLike {leftExpr :: Expr, rightExp :: Expr} -- TODO: only Col and Const? (String add???)

data Expr
    = Col String
    | Const Cell
    | Operation {left :: Expr, op:: Expr -> Expr -> Expr, right :: Expr}

containsRet :: (SchemaItem -> Bool) -> [SchemaItem] -> (Bool, Cell)
containsRet f list = if not (null filtered)
    then (True, snd . head $ filtered)
    else (False, CStr "")
    where filtered = filter f list

-- returns true if both expressions have compatible types
-- types of expressions are expressed using the Cell type and
-- compatible judge is the function comparing those cells
exprCompat :: Schema -> Expr -> Expr -> (Cell -> Cell -> Bool) -> Bool
exprCompat contextSchema (Col colName1) (Col colName2) compatibleJudge =
    bothPresent && compatibleJudge fstCell sndCell
  where
    (fstPresent, fstCell) = containsRet (\x -> fst x == colName1) contextSchema
    (sndPresent, sndCell) = containsRet (\x -> fst x == colName2) contextSchema
    bothPresent           = fstPresent && sndPresent
exprCompat contextSchema (Col colName1) (Const cellVal) compatibleJudge =
    fstPresent && compatibleJudge fstCell cellVal
  where
    (fstPresent, fstCell) = containsRet (\x -> fst x == colName1) contextSchema
exprCompat contextSchema (Const cellVal1) (Const cellVal2) compatibleJudge =
    compatibleJudge cellVal1 cellVal2
exprCompat s const col j = exprCompat s col const j

-- WIP condition evaluation (don't know how the expr will be friendly with the parser, so TODO until parsing is done?)
-- TODO evaluation context includes both schema and row
-- rely on the fact that row conforms to schema??? (should be ok as long as initial parsing is correct)  
-- evalCond :: Schema -> Row -> Conditional -> Bool
-- evalCond s row (And (l, r)) = evalCond s row l && evalCond s row r
-- evalCond s row (Or  (l, r)) = evalCond s row l || evalCond s row r
-- evalCond s row (Xor (l, r)) = (le || re) && ((not le && re) || (not re && le))
--   where
--     le = evalCond s row l
--     re = evalCond s row r
-- evalCond s             row (Not x     ) = not (evalCond s row x)
-- -- TODO
-- -- evalCond s             row (NotE expr) = if not compatible
-- --     then error "Incompatible types for equality operator"
-- --     else False
-- --     where compatible = exprCompat contextSchema l r compatibleEq
-- evalCond contextSchema row (Eq  (l, r)) = if not compatible
--     then error "Incompatible types for equality operator or row not adhering to schema"
--     else False -- TODO l == r
--     where compatible = exprCompat contextSchema l r compatibleEq
-- evalCond contextSchema row (Less  (l, r)) = if not compatible
--     then error "Incompatible types for equality operator or row not adhering to schema"
--     else False -- TODO l < r
--     where compatible = exprCompat contextSchema l r compatibleOrd
-- evalCond contextSchema row (Greater  (l, r)) 
--     | not compatible =  error "Incompatible types for equality operator or row not adhering to schema"
--     | not rowCompatible = 
--     else False -- TODO l > r, first attempt:
--     where 
--         compatible = exprCompat contextSchema l r compatibleOrd
--         idx c = elemIndex c (map fst contextSchema)
--         extractCell (Col c) = row !! (idx c)
