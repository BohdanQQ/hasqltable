module Types where
import           Data.List                      ( nub )

data Cell
  = CStr String
  | CInt Integer
  | CDouble Double
  | CBool Bool

type SchemaItem = (String, Cell)

type Schema = [SchemaItem]

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
    _           == _           = error "Cannot compare incompatible cells"

instance (Ord Cell) where
    compare (CStr    a) (CStr    b) = compare a b
    compare (CInt    a) (CInt    b) = compare a b
    compare (CDouble a) (CDouble b) = compare a b
    compare (CInt    a) (CDouble b) = compare (fromInteger a) b
    compare (CDouble a) (CInt    b) = compare a (fromInteger b)
    compare (CBool   a) (CBool   b) = compare a b
    compare _           _           = error "Cannot compare incompatible cells"

instance (Show Cell) where
    show (CStr    a) = 'S' : show a
    show (CInt    a) = 'I' : show a
    show (CDouble a) = 'D' : show a
    show (CBool   a) = 'B' : show a

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
