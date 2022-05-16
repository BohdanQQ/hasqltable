module Main where
import           Control.Monad                  ( forever )
import           Data.List
import           Data.List.Split
import           Operations                     ( execute
                                                , parseCsv
                                                , parseQuery
                                                , prettyPrintTable
                                                )
import           System.Environment
import           System.IO
import           Types                          ( Cell(..)
                                                , Expr(..)
                                                , Order(..)
                                                , add
                                                , div
                                                , evalExpr
                                                , mul
                                                , parseCell
                                                , sub
                                                , typeOfCell
                                                , unwrapOrErr
                                                )

printResult t (Just subqs) = do
    print subqs
    printResult' (execute subqs (Right t))
printResult _ Nothing = putStrLn "cannot parse query"
printResult' (Left e) = do
    putStrLn e
printResult' (Right t) = do
    prettyPrintTable t

main :: IO ()
main = do
    args <- getArgs
    let (path, schemaDesc, parserProperty) = parseArgs args
    handle <- openFile path ReadMode
    table  <- parseCsv parserProperty schemaDesc handle
    putStrLn ""
    prettyPrintTable table

    forever
        (   getLine
        >>= (\ln -> putStrLn "RESULT" >> printResult table (parseQuery ln))
        )
    -- putStrLn
    --     (  "expr: "
    --     ++ (show
    --            (evalExpr [("alpha", CStr ""), ("beta", CInt 0)]
    --                      [CStr "value", CInt 45]
    --                      (Col "beta")
    --            )
    --        )
    --     )
    -- putStrLn
    --     (  "expr: "
    --     ++ (show
    --            (evalExpr [("alpha", CStr ""), ("beta", CInt 0)]
    --                      [CStr "value", CInt 45]
    --                      (Const (CInt 45))
    --            )
    --        )
    --     )
    -- -- putStrLn ("expr: " ++ (show (evalExpr [("alpha", CStr ""), ("beta", CInt 0)] [CStr "value", CBool False] (Col "beta"))))
    -- putStrLn
    --     (  "expr: "
    --     ++ (show
    --            (evalExpr
    --                [ ("alpha", CStr "")
    --                , ("beta" , CInt 0)
    --                , ("gamma", CDouble 0)
    --                ]
    --                [CStr "value", CInt 45, CDouble 3.14]
    --                (Operation (Col "beta") add (Const (CBool False)))
    --            )
    --        )
    --     )
    -- putStrLn
    --     (  "expr: "
    --     ++ (show
    --            (evalExpr [("alpha", CStr ""), ("beta", CInt 0)]
    --                      [CStr "value", CInt 45]
    --                      (Col "beta")
    --            )
    --        )
    --     )
    hClose handle

parseArgs args | argCount < 3 = error "Invalid Argument Count"
               | otherwise    = (head args, args !! 1, args !! 2)
    where argCount = length args
