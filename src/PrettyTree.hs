module PrettyTree where

import Parser.AST

--------------------------------------------------------------------------------
-- Funkcje do ładnego wypisywania (Pretty Printer)
--------------------------------------------------------------------------------

-- Pomocnicza funkcja do tworzenia wcięć i "gałęzi" drzewa
printNode :: Int -> String -> IO ()
printNode level label = do
    let indent = concat $ replicate level "|   "
    putStrLn $ indent ++ "|-- " ++ label

-- 1. Wypisywanie Programu (listy funkcji)
printTree :: Program -> IO ()
printTree [] = putStrLn "Empty Program"
printTree funcs = mapM_ printFunction funcs

-- 2. Wypisywanie Funkcji
printFunction :: FunctionDecl -> IO ()
printFunction f = do
    putStrLn $ "FunctionDecl: " ++ funcSymbol f ++ " (returns " ++ show (funcRetType f) ++ ")"
    
    -- Wypisz argumenty
    if null (funcArgs f)
        then printNode 1 "Args: None"
        else do
            printNode 1 "Args:"
            mapM_ (\(t, n) -> printNode 2 (n ++ " :: " ++ show t)) (funcArgs f)
    
    -- Wypisz ciało funkcji (instrukcje)
    printNode 1 "Body:"
    if null (funcBody f) 
        then printNode 2 "(empty body)"
        else mapM_ (printStmt 2) (funcBody f)
    
    putStrLn "" -- Pusta linia między funkcjami

-- 3. Wypisywanie Instrukcji (Stmt)
printStmt :: Int -> Stmt -> IO ()

-- Return
printStmt lvl (Ret expr) = do
    printNode lvl "Return"
    printExpr (lvl + 1) expr

-- Deklaracja zmiennej
printStmt lvl (VarDecl t name expr) = do
    printNode lvl $ "VarDecl: " ++ name ++ " :: " ++ show t
    printExpr (lvl + 1) expr

printStmt lvl (FuncCall s args ) = do
    printNode lvl $ "FuncCall: " ++ s
    mapM_ (printExpr (lvl+2)) args

-- Samodzielne wyrażenie (np. wywołanie funkcji, przypisanie)
printStmt lvl (ExprStmt expr) = do
    printNode lvl "ExprStmt"
    printExpr (lvl + 1) expr

-- Instrukcja IF
printStmt lvl (IfStmt cond trueStmts falseStmts) = do
    printNode lvl "If Statement"

    printNode (lvl + 1) "Condition:"
    printExpr (lvl + 2) cond

    printNode (lvl + 1) "Then Block:"
    if null trueStmts
        then printNode (lvl + 2) "(empty)"
        else mapM_ (printStmt (lvl + 2)) trueStmts

    if null falseStmts
        then return ()
        else do
            printNode (lvl + 1) "Else Block:"
            mapM_ (printStmt (lvl + 2)) falseStmts

-- Instrukcja WHILE (Dodana)
printStmt lvl (WhileStmt cond bodyStmts) = do
    printNode lvl "While Loop"
    
    printNode (lvl + 1) "Condition:"
    printExpr (lvl + 2) cond
    
    printNode (lvl + 1) "Body:"
    if null bodyStmts
        then printNode (lvl + 2) "(empty)"
        else mapM_ (printStmt (lvl + 2)) bodyStmts

-- Instrukcja FOR (Uzupełniona)
printStmt lvl (ForStmt init cond step bodyStmts) = do
    printNode lvl "For Loop"
    
    printNode (lvl + 1) "Init:"
    printForInit (lvl + 2) init
    
    printNode (lvl + 1) "Condition:"
    printExpr (lvl + 2) cond
    
    printNode (lvl + 1) "Step:"
    printExpr (lvl + 2) step
    
    printNode (lvl + 1) "Body:"
    if null bodyStmts
        then printNode (lvl + 2) "(empty)"
        else mapM_ (printStmt (lvl + 2)) bodyStmts

-- 4. Pomocnicza funkcja do Inicjalizacji w FOR
printForInit :: Int -> ForInit -> IO ()
printForInit lvl (InitDecl t name expr) = do
    printNode lvl $ "InitDecl: " ++ name ++ " :: " ++ show t
    printExpr (lvl + 1) expr
printForInit lvl (InitExpr expr) = do
    printNode lvl "InitExpr"
    printExpr (lvl + 1) expr
printForInit lvl NoInit = do
    printNode lvl "NoInit"

-- 5. Wypisywanie Wyrażeń (Expr)
printExpr :: Int -> Expr -> IO ()
printExpr lvl (IExp i) = printNode lvl $ "Int:    " ++ show i
printExpr lvl (FExp f) = printNode lvl $ "Float:  " ++ show f
printExpr lvl (CExp c) = printNode lvl $ "Char:   '" ++ [c] ++ "'"
printExpr lvl (SExp s) = printNode lvl $ "Symbol: \"" ++ s ++ "\"" -- Zmienione na cudzysłów dla jasności

printExpr lvl (Assign name expr) = do
    printNode lvl $ "Assign To: " ++ name
    printExpr (lvl + 1) expr

printExpr lvl (UnaryExp op e) = do
    printNode lvl $ "UnaryOp: " ++ show op
    printExpr (lvl + 1) e

printExpr lvl (BinExp op e1 e2) = do
    printNode lvl $ "BinOp:   " ++ show op
    printExpr (lvl + 1) e1
    printExpr (lvl + 1) e2
