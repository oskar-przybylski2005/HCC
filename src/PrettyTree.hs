module PrettyTree where

import HParser

--------------------------------------------------------------------------------
--  PUBLIC API
--------------------------------------------------------------------------------

printProgram :: Program -> IO ()
printProgram prog = putStrLn $ programAsString $ prog

programAsString :: Program -> String
programAsString funcs =
    unlines $
      ["Program"] ++
      concat (zipWith (\f isLast -> printFunc f "" isLast)
                      funcs
                      (lastFlags funcs))

-- Ustalanie, które elementy są ostatnie
lastFlags :: Eq a => [a] -> [Bool]
lastFlags xs = map (== last xs) xs

-- Drukowanie Function
printFunc :: Function -> String -> Bool -> [String]
printFunc (Function ret name args body) prefix isLast =
    header : subtrees
  where
    connector = if isLast then "^-- " else "|-- "
    newPrefix = if isLast then prefix ++ "    "
                          else prefix ++ "|   "

    header = prefix ++ connector ++ "Function: " ++ name

    subtrees =
        concat
            [ printLeaf  newPrefix "Return type" (show ret) False
            , printBody  newPrefix body
            , printArgs  newPrefix args
            ]

-- Print list of arguments
printArgs :: String -> [Arg] -> [String]
printArgs prefix [] =
    [prefix ++ "^-- Args: (none)"]
printArgs prefix args =
    (prefix ++ "|-- Args") :
      concat (zipWith printArg args (lastFlags args))
  where
    printArg (t, sym) isLast =
        let conn = if isLast then "^-- " else "|-- "
        in [prefix ++ "    " ++ conn ++ show t ++ " " ++ sym]

-- Print list of statements
printBody :: String -> [Stmt] -> [String]
printBody prefix [] =
    [prefix ++ "^-- Body: (empty)"]
printBody prefix stmts =
    (prefix ++ "|-- Body") :
      concat (zipWith printStmt stmts (lastFlags stmts))

printStmt :: Stmt -> Bool -> [String]
printStmt stmt isLast =
    let conn = if isLast then "|-- " else "^-- "
    in ["    " ++ conn ++ show stmt]

-- Utility for single leaf (Return type)
printLeaf :: String -> String -> String -> Bool -> [String]
printLeaf prefix label value isLast =
    let conn = if isLast then "^-- " else "|-- "
    in [prefix ++ conn ++ label ++ ": " ++ value]
