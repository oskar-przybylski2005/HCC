module Parser where

import Lexer

-- C Language CFG blueprint
-- program -> {function}+
-- function -> type ident "(" [params] ")" cmp_stmt
-- params -> param {"," param}*
-- param -> type ident
-- type -> "int"
--       | "short"
--       | "long"
--       | "void"
--       | "float"
--       | "char"
--       | "double"
--
-- cmp_stmt -> "{" {stmt}* "}"
-- stmt -> expr_stmt
--       | if_stmt
--       | while_stmt
--       | return_stms
--       | cmp_stmt
--
-- expr_stmt -> expr ";"
-- if_stmt -> "if" "(" expr ")" stmt ["else" stmt]
-- while_stmt -> "while" "(" expr ")" stmt
-- return_stms -> "return" [expr] ";"
-- decl_stmt -> type ident ["=" expr] ";"
-- expr -> expr "+" term
--       | expr "-" term
--       | term
--
-- term -> term "*" factor
--       | term "/" factor
--       | factor
--
-- factor -> "(" expr ")"
--        | number
--        | ident
--        | ident "(" [args] ")"
--
-- args -> expr {"," expr}*
--
-- number -> [0-9]+
-- ident  -> [a-zA-Z_][a-zA-Z0-9_]*
--

type Program = [Function]
type Symbol = String

data Function = Function {
    retType :: Type,
    symbol  :: Symbol,
    args    :: [Arg],
    body    :: [Stmt]
} deriving (Show,Eq)

type Arg  = (Type, Symbol)

data Stmt = Ret Expr
          | Assign Type Symbol Expr
    deriving (Show,Eq)

type Expr = Integer

data Type = TInt
          | TVoid
          | TFloat
          | TChar
    deriving (Show,Eq)

type Parser a = [Token] -> Maybe (a, [Token])

parseExpr :: Parser Expr
parseExpr (TokenIntLit v : rest) = Just (read v , rest)
parseExpr _ = Nothing

parseStmts :: Parser [Stmt]
parseStmts (TokenBraR : rest) = Just ([], rest)
parseStmts (TokenNewLine: rest) = parseStmts rest
parseStmts (TokenKeyWord typ : TokenSymbol name :
            TokenEq : rest) = do
             (expr, afterExpr) <- parseExpr rest
             let rest' = case afterExpr of
                    (TokenSemi : r ) -> r
                    e -> error ("Expected ; got"++ show e)
             (stmts, afterStmts) <- parseStmts rest'
             case typ of
                ("int") -> Just (Assign TInt name expr : stmts, afterStmts)
                ("char") -> Just (Assign TChar name expr : stmts, afterStmts)
                ("float") -> Just (Assign TFloat name expr : stmts, afterStmts)

parseStmts (TokenKeyWord "return" : TokenSemi : rest) = parseStmts rest
parseStmts (TokenKeyWord "return" : rest) = do
             (expr,  afterExpr)  <- parseExpr rest
             let rest' = case afterExpr of
                    (TokenSemi : r ) -> r
                    e -> error ("Expected ; got"++ show e)
             (stmts, afterStmts) <- parseStmts rest'
             Just (Ret expr : stmts, afterStmts)
parseStmts _ = Nothing

parseArgs :: Parser [Arg]
parseArgs (TokenParR : rest) = Just ([], rest)
parseArgs (TokenComma: rest) = parseArgs rest
parseArgs (TokenKeyWord "void" : TokenParR : rest) = Just([], rest)
parseArgs (TokenKeyWord t : TokenSymbol name : rest) = do
    (args, rest') <- parseArgs rest

    let argtype = case t of
            "int"  -> TInt
            "void" -> TVoid
            "char" -> TChar
            "float" -> TFloat
            _      -> error "wrong func arg type"

    Just ((argtype, name) : args, rest')
parseArgs _ = Nothing

consumeNL :: [Token] -> [Token]
consumeNL (TokenNewLine : rest) = consumeNL rest
consumeNL (TokenBraL: rest) = rest
consumeNL t = t

parseFunction :: Parser Function
parseFunction (TokenKeyWord t   :
               TokenSymbol name :
               TokenParL : rest ) = do
               -- from ( to )
               (args', body)   <- parseArgs  rest
               -- from { to }
               let body' = consumeNL body
               (stmts,rest')   <- parseStmts body'
               let rt = case t of
                    ("int")  -> TInt
                    ("void") -> TVoid
                    ("char") -> TChar
                    ("float") -> TFloat
                    _ -> error "Wrong function return type"
               let func = Function {
                   retType = rt,
                   symbol  = name,
                   args    = args',
                   body    = stmts
               }
               Just(func, rest')
parseFunction _ = Nothing

parse :: Parser Program
parse (TokenEnd : []) = Just ([] , [])
parse (TokenNewLine : rest) = parse rest
parse s@(TokenKeyWord _ : _) = do
    (function, rest )  <- parseFunction s
    (functions,rest')  <- parse rest
    Just(function : functions, rest')

parse _ = Nothing
