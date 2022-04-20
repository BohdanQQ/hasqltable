module Main where

import System.Exit (exitFailure)
import Types 
import Data.Text (isInfixOf, pack, toLower)


printAndExit message = do
    putStrLn message
    exitFailure

assertContains result@(success, errMsg) ex@(expected, errPart) helper =
  if success == expected
    then
      if not containsErrMsg
        then printAndExit $ "\n\n--- Unexpected message\n\n" ++ errMsg ++ "\n\n--- when expecting:\n\n" ++ show ex ++ "---\n\n" ++ helper
        else putStr ""
    else printAndExit $ "\n\n--- Unexpected result:\n\n" ++ show result ++ "\n\n--- when expecting:\n\n" ++ show ex ++ "---\n\n" ++ helper
  where
    containsErrMsg = (Data.Text.toLower . Data.Text.pack $ errPart) `Data.Text.isInfixOf` (Data.Text.toLower . Data.Text.pack $ errMsg)

queryChecktests =
  [ ( -- empty select
      queryCheck [Select []],
      (False, "empty")
    ),
    ( -- invalid start
      queryCheck [Limit 2],
      (False, "begin")
    ),
    ( queryCheck [Select ["aleph"]],
      (True, "")
    ),
    ( queryCheck [Select ["aleph"], Where (["x", "a"], const False)],
      (True, "")
    ),
    ( -- no duplicates in where
      queryCheck [Select ["aleph"], Where (["aaa", "aaa"], const False)],
      (False, "duplicate")
    ),
    ( queryCheck [Select ["aleph"], GroupBy ["", "a"]],
      (True, "")
    ),
    ( -- empty groupby
      queryCheck [Select ["aleph"], GroupBy []],
      (False, "empty")
    ),
    ( -- empty orderby
      queryCheck [Select ["aleph"], OrderBy (Asc, [])],
      (False, "empty")
    ),
    ( queryCheck [Select ["aleph"], Limit 0],
      (True, "nonnegative")
    ),
    ( -- limit must be used with nonnegative number
      queryCheck [Select ["aleph"], Limit (-1)],
      (False, "nonnegative")
    ),
    ( queryCheck [Select ["aleph"], Where (["", "a"], const False), GroupBy ["alpeh"], OrderBy (Asc, ["a"]), Limit 1],
      (True, "")
    ),
    ( -- empty orderBy
         queryCheck [Select ["aleph"], Where (["", "a"], const False), GroupBy ["alpeh"], OrderBy (Asc, []), Limit 1],
      (True, "")
    ),
    ( -- duplicate clause
      queryCheck [Select ["aleph"], Where (["", "a"], const False), GroupBy ["alpeh"], GroupBy ["alpeh"], OrderBy (Desc, ["a"]), Limit 1],
      (False, "end")
    ),
    ( -- invalid order, invalid having position
      queryCheck [Select ["aleph"], Where (["", "a"], const False), OrderBy (Asc, ["a"]), Limit 1, OrderBy (Asc, ["a"])],
      (False, "last")
    )
  ]


testQueryCheck = map (\(number, (input, expected)) -> assertContains input expected ("Test number (1-indexed): QueryCheck - " ++ show number)) (zip [1..] queryChecktests)

main = do
    last testQueryCheck
    