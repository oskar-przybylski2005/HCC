module PrettyTree where

import Parser

--------------------------------------------------------------------------------
--  PUBLIC API
--------------------------------------------------------------------------------

printProgram :: Program -> IO ()
printProgram prog = putStrLn (unlines (treeProgram prog))

--------------------------------------------------------------------------------
--  TREE BUILDERS
--------------------------------------------------------------------------------

treeProgram :: Program -> [String]
treeProgram funcs =
    concatMap (\f -> treeNode True ("Function: " ++ symbol f) (treeFunction f)) funcs


treeFunction :: Function -> [String]
treeFunction (Function t name args body) =
    [ "Return type: " ++ show t ] ++
    [ "Args:" ] ++
    treeArgs args ++
    [ "Body:" ] ++
    concatMap treeStmt body


treeArgs :: [Arg] -> [String]
treeArgs [] = ["(none)"]
treeArgs xs = concatMap render xs
  where
    render (t, s) = treeNode False (s ++ " : " ++ show t) []


treeStmt :: Stmt -> [String]
treeStmt (Ret expr) =
    treeNode False "Return" (treeExpr expr)
treeStmt (Assign typ name expr) =
    treeNode False ("Assign " ++ show typ ++ " "++ name) (treeExpr expr)

treeExpr :: Expr -> [String]
treeExpr n = treeNode False (show n) []


--------------------------------------------------------------------------------
--  GENERIC TREE PRIMITIVES
--------------------------------------------------------------------------------
-- treeNode controls the hierarchical tree ASCII style.
-- 
--  isLast = True  → node is final child → "└──"
--  isLast = False → node has siblings  → "├──"
--
--  children are indented properly.

treeNode :: Bool -> String -> [String] -> [String]
treeNode isLast label children =
    let prefix = if isLast then "└── " else "├── "
        childPrefix = if isLast then "    " else "│   "
    in (prefix ++ label)
       : indent childPrefix children

indent :: String -> [String] -> [String]
indent pref = map (pref ++)
