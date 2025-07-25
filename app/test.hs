module Main where

import           Data.Text                      ( isInfixOf
                                                , pack
                                                , toLower
                                                )
import           Operations
import           Parsing
import           System.Exit                    ( exitFailure )
import           Types

printAndExit message = do
    putStrLn message
    exitFailure

assertContains result@(success, errMsg) ex@(expected, errPart) helper =
    if success == expected
        then if not containsErrMsg
            then
                printAndExit
                $  "\n\n--- Unexpected message\n\n"
                ++ errMsg
                ++ "\n\n--- when expecting:\n\n"
                ++ show ex
                ++ "\n\n---\n\n"
                ++ helper
            else putStr ""
        else
            printAndExit
            $  "\n\n--- Unexpected result:\n\n"
            ++ show result
            ++ "\n\n--- when expecting:\n\n"
            ++ show ex
            ++ "\n\n---\n\n"
            ++ helper
  where
    containsErrMsg =
        (Data.Text.toLower . Data.Text.pack $ errPart)
            `Data.Text.isInfixOf` (Data.Text.toLower . Data.Text.pack $ errMsg)

assertEq result ex helper = if result == ex
    then putStr ""
    else
        printAndExit
        $  "\n\n--- Unexpected result:\n\n"
        ++ show result
        ++ "\n\n--- when expecting:\n\n"
        ++ show ex
        ++ "\n\n---\n\n"
        ++ helper

-- | an old query checking code implemented before parsing

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

queryChecktests =
    [ ( -- empty select
       queryCheck [Select []]                          , (False, "empty"))
    , ( -- invalid start
       queryCheck [Limit 2]                            , (False, "begin"))
    , (queryCheck [Select ["aleph"]]                   , (True, ""))
    , (queryCheck [Select ["aleph"], Where (Const (CBool False))], (True, ""))
    , (queryCheck [Select ["aleph"], GroupBy ["", "a"]], (True, ""))
    , ( -- empty groupby
       queryCheck [Select ["aleph"], GroupBy []]       , (False, "empty"))
    , ( -- empty orderby
       queryCheck [Select ["aleph"], OrderBy (Asc, [])], (False, "empty"))
    , (queryCheck [Select ["aleph"], Limit 0]          , (True, ""))
    , ( -- limit must be used with nonnegative number
       queryCheck [Select ["aleph"], Limit (-1)]       , (False, "nonnegative"))
    , ( queryCheck
          [ Select ["aleph"]
          , Where (Const (CBool False))
          , GroupBy ["alpeh"]
          , OrderBy (Asc, ["a"])
          , Limit 1
          ]
      , (True, "")
      )
    , ( -- empty orderBy
        queryCheck
          [ Select ["aleph"]
          , Where (Const (CBool False))
          , GroupBy ["alpeh"]
          , OrderBy (Asc, [])
          , Limit 1
          ]
      , (False, "empty")
      )
    , ( -- duplicate clause
        queryCheck
          [ Select ["aleph"]
          , Where (Const (CBool False))
          , GroupBy ["alpeh"]
          , GroupBy ["alpeh"]
          , OrderBy (Desc, ["a"])
          , Limit 1
          ]
      , (False, "end")
      )
    , ( -- invalid order, invalid having position
        queryCheck
          [ Select ["aleph"]
          , Where (Const (CBool False))
          , OrderBy (Asc, ["a"])
          , Limit 1
          , OrderBy (Asc, ["a"])
          ]
      , (False, "last")
      )
    ]


testQueryCheck = testOn
    (\(number, (input, expected)) -> assertContains
        input
        expected
        ("Test number (1-indexed): QueryCheck - " ++ show number)
    )
    queryChecktests

iType = CInt 0
dType = CDouble 0
sType = CStr ""
bType = CBool False

cellParseTests :: [((SchemaType, String), Maybe Cell)]
cellParseTests =
    [ ((SInt, "1")          , Just (CInt 1))
    , ((SInt, "ydsthdvasbd"), Nothing)
    , ((SDouble, "1.0")        , Just (CDouble 1.0))
    , ((SDouble, "ydsthdvasbd"), Nothing)
    , ((SStr, "1")          , Just (CStr "1"))
    , ((SBool, "True")       , Just (CBool True))
    ]

testCellParse = testOn
    (\(number, ((in1, in2), expected)) -> assertEq
        (parseCell in1 in2)
        expected
        ("Test number (1-indexed): CellParse - " ++ show number)
    )
    cellParseTests

createEvalTest lctor rctor left op right =
    Operation (Const (lctor left)) op (Const (rctor right))

evalInt = createEvalTest CInt CInt
evalDouble = createEvalTest CDouble CDouble
evalBool = createEvalTest CBool CBool


exprEvalNoTableTests =
    [ (evalInt 3 add 2                          , CInt 5)
    , (evalDouble 3 add 2                       , CDouble 5)
    , (evalDouble 2 sub 3                       , CDouble (-1))
    , (evalDouble 5 Types.div 2                 , CDouble 2.5)
    , (evalInt 5 mul 8                          , CInt 40)
    , (evalBool True boolAnd False              , CBool False)
    , (evalBool True boolOr False               , CBool True)
    , (createEvalTest CInt CDouble 2 sub 5      , CDouble (-3))
    , (createEvalTest CDouble CInt 2 Types.div 5, CDouble 0.4)
    ]

testEvalNoTable :: [IO ()]
testEvalNoTable = zipWith
    (curry
        (\(number, (input, expected)) ->
            let message =
                    (  "Test number (1-indexed): EvalExpr - no schema/table - "
                    ++ show number
                    )
            in  case evalExpr [] [] input of
                    Left  e -> putStrLn (message ++ e) >> exitFailure
                    Right r -> assertEq r expected message
        )
    )
    [1 ..]
    exprEvalNoTableTests



nop :: Cell -> Cell -> Either a Cell
nop _ _ = Right (CBool True)
parseQueryTests =
    [ ("SELECT x"              , Just [Select ["x"]])
    , ("select x"              , Just [Select ["x"]])
    , ("sElEcT x"              , Just [Select ["x"]])
    , ("SELECT x, y"           , Just [Select ["x", "y"]])
    , ("SELECT x    , y     "  , Just [Select ["x", "y"]])
    , ("SELECT x, `y`, `z`"    , Just [Select ["x", "y", "z"]])
    , ("SELECT x,   `y`,   `z`", Just [Select ["x", "y", "z"]])
    , ("SELECT where"          , Nothing)
    , ("selec where"           , Nothing)
    , ("select `x`"            , Just [Select ["x"]])
    , ("SELECT x LIMIT 23"     , Just [Select ["x"], Limit 23])
    , ( "SELECT x, y, `z`, `LIMIT` LIMIT 5"
      , Just [Select ["x", "y", "z", "LIMIT"], Limit 5]
      )
    , ("SELECT x, y, `z`, LIMIT 5", Nothing)
    , ("SELECT x, y LIMIT -5"     , Nothing)
    , ( "select x where `b` == \"b\""
      , Just [Select ["x"], Where (Operation (Col "b") nop (Const (CStr "b")))]
      )
    , ( "select x where `a` + 4.5 <= 56.4 / `b`"
      , Just
          [ Select ["x"]
          , Where
              (Operation (Operation (Col "a") nop (Const (CDouble 4.5)))
                         nop
                         (Operation (Const (CDouble 56.4)) nop (Col "b"))
              )
          ]
      )
    , ("select x where `a` + 4.5 <= 56.4 /", Nothing)
    , ("select x where `a` + <="           , Nothing)
    , ("select x where `a` <="             , Nothing)
    , ( "select x where `a` + 4.5 <= 56.4 / `b` limit 5"
      , Just
          [ Select ["x"]
          , Where
              (Operation (Operation (Col "a") nop (Const (CDouble 4.5)))
                         nop
                         (Operation (Const (CDouble 56.4)) nop (Col "b"))
              )
          , Limit 5
          ]
      )
    , ("select x orderby asc y", Just [Select ["x"], OrderBy (Asc, ["y"])])
    , ("select x orderby asc"      , Nothing)
    , ("select x orderby asc where", Nothing)
    , ("select x orderby asc y, z, `alpha`"
      , Just [Select ["x"], OrderBy (Asc, ["y", "z", "alpha"])]
      )
    , ("select x orderby asc y, where, `alpha`", Nothing)
    , ( "select x where True orderby asc y, z, `alpha`"
      , Just
          [ Select ["x"]
          , Where (Const (CBool True))
          , OrderBy (Asc, ["y", "z", "alpha"])
          ]
      )
    , ("select x orderby asc y, z, `alpha` where True ", Nothing)
    ]

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

instance Show Expr where
    show (Col   c        ) = "ColExpr(" ++ show c ++ ")"
    show (Const c        ) = "ConsExpr(" ++ show c ++ ")"
    show (Operation l _ r) = "(" ++ show l ++ "?" ++ show r ++ ")"

instance Eq Expr where
    (Col   c        ) == (Col   d          ) = c == d
    (Const c        ) == (Const d          ) = c == d
    (Operation l _ r) == (Operation l1 _ r1) = l == l1 && r == r1
    _                 == _                   = False

testParseQuery = testOn
    (\(number, (input, expected)) -> assertEq
        (parseQuery input)
        expected
        (  "Test number (1-indexed): parseSelect - no schema/table - "
        ++ show number
        ++ " with input\n"
        ++ input
        )
    )
    parseQueryTests


testOn :: ((Int, a) -> b) -> [a] -> [b]
testOn fn = zipWith (curry fn) [1 ..]

runTests :: [IO ()] -> IO ()
runTests = foldl (>>) (putStr "")


main :: IO ()
main = do
    runTests testQueryCheck
    runTests testCellParse
    runTests testEvalNoTable
    runTests testParseQuery
