module Main where
import           Data.List
import           Data.List.Split
import           Operations                     ( prettyPrintTable )
import           System.Environment
import           System.IO
import           Types                          ( Cell(..)
                                                , parseCell
                                                , typeOfCell
                                                , unwrapOrErr
                                                )

main :: IO ()
main = do
    args <- getArgs
    let argsLen            = length args
    let (path, schemaDesc) = parseArgs args
    handle   <- openFile path ReadMode
    contents <- hGetContents handle
    let rawRows   = lines contents
    let splitRows = map (splitOn ",") rawRows
    let header    = head splitRows
    let body      = tail splitRows
    let schema    = createDefualtSchema header schemaDesc
    let schemaCellOrder = map snd schema
    let tbody     = map (parseWithSchema schemaCellOrder) body
    putStrLn (show schema)
    putStrLn (show tbody)
    putStrLn ""
    prettyPrintTable (schema, [tbody])
    hClose handle

parseArgs args | argCount < 2 = error "Invalid Argument Count"
               | otherwise    = (head args, args !! 1)
    where argCount = length args

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

createDefualtSchema names repr = if valid
    then zipWith createSchemaCell names repr
    else error "Invalid schema specification/column names"
  where
    valid =
        all (\c -> c == 'i' || c == 'd' || c == 'b' || c == 's') repr
            && nub names == names
            && length names == length repr
    createSchemaCell name c = case c of
        'i' -> (name, CInt 0)
        's' -> (name, CStr "")
        'd' -> (name, CDouble 0)
        'b' -> (name, CBool False)
        _   -> error "Invalid specification"
