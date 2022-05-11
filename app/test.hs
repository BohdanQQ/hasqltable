module Main where

import           Data.Text                      ( isInfixOf
                                                , pack
                                                , toLower
                                                )
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

queryChecktests =
    [ ( -- empty select
       queryCheck [Select []]       , (False, "empty"))
    , ( -- invalid start
       queryCheck [Limit 2]         , (False, "begin"))
    , (queryCheck [Select ["aleph"]], (True, ""))
    , ( queryCheck [Select ["aleph"], Where (["x", "a"], const False)]
      , (True, "")
      )
    , ( -- no duplicates in where
        queryCheck [Select ["aleph"], Where (["aaa", "aaa"], const False)]
      , (False, "duplicate")
      )
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
          , Where (["", "a"], const False)
          , GroupBy ["alpeh"]
          , OrderBy (Asc, ["a"])
          , Limit 1
          ]
      , (True, "")
      )
    , ( -- empty orderBy
        queryCheck
          [ Select ["aleph"]
          , Where (["", "a"], const False)
          , GroupBy ["alpeh"]
          , OrderBy (Asc, [])
          , Limit 1
          ]
      , (False, "empty")
      )
    , ( -- duplicate clause
        queryCheck
          [ Select ["aleph"]
          , Where (["", "a"], const False)
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
          , Where (["", "a"], const False)
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

cellParseTests =
    [ ((iType, "1")          , Just (CInt 1))
    , ((iType, "ydsthdvasbd"), Nothing)
    , ((dType, "1.0")        , Just (CDouble 1.0))
    , ((dType, "ydsthdvasbd"), Nothing)
    , ((sType, "1")          , Just (CStr "1"))
    , ((bType, "True")       , Just (CBool True))
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
    [
        (
            evalInt 3 add 2,
            CInt 5
        ),
        (
            evalDouble 3 add 2,
            CDouble 5
        ),
        (
            evalDouble 2 sub 3,
            CDouble (-1)
        ),
        (
            evalDouble 5 Types.div 2,
            CDouble 2.5
        ),
        (
            evalInt 5 mul 8,
            CInt 40
        ),
        (
            evalBool True boolAnd False,
            CBool False
        ),
        (
            evalBool True boolOr False,
            CBool True
        ),
        (
            createEvalTest CInt CDouble 2 sub 5,
            CDouble (-3)
        ),
        (
            createEvalTest CDouble CInt 2 Types.div 5,
            CDouble 0.4
        )
    ]

testEvalNoTable :: [IO ()]
testEvalNoTable = testOn 
    (\(number, (input, expected)) -> assertEq
        (evalExpr [] [] input)
        expected
        ("Test number (1-indexed): EvalExpr - no schema/table - " ++ show number)
    )
    exprEvalNoTableTests

testOn :: ((Int, a) -> b) -> [a] -> [b]
testOn fn = zipWith (curry fn) [1..]

main = do
    foldl (>>) (putStr "") testQueryCheck
    foldl (>>) (putStr "") testCellParse
    foldl (>>) (putStr "") testEvalNoTable
