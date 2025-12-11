module Parser.AST where

-- Supported C Language Backus-Naur Form blueprint
-- <program>  ::= <function>
--
-- <type>     ::= "int" | "float" | "void" | "char"
--
-- <function> ::= <type> <id> "(" [<arg>] ")" "{" [<stmt>] "}"
--
-- <arg>      ::= <type> <id> {","}
--
-- <stmt>     ::= "return" {<expr>} ";"
--             |  <type> <id> "=" <expr> ";"
--             |  <id> "(" [ <expr> {","} ] ")" ";" -- TODO function calls
--
-- <expr>     ::= <int> | <float>

-- AST types
type Program = [FunctionDecl]
data FunctionDecl = FunctionDecl {
    funcRetType :: Type,
    funcSymbol  :: String,
    funcArgs    :: [Arg],
    funcBody    :: [Stmt]
} deriving (Show,Eq)

type Arg  = (Type, String)

data Stmt = Ret Expr
          | VarDecl  Type   String Expr
          | FuncCall String [Expr]
          | ExprStmt Expr
          | IfStmt {
             iCondition  ::  Expr,
             iBodyBlock  :: [Stmt],
             iElseBlock  :: [Stmt]
          }
          | ForStmt{
              fInit      ::  ForInit,
              fCondition ::  Expr,
              fStep      ::  Expr,
              fBody      :: [Stmt]
          }
          | WhileStmt{
              wCond      ::  Expr,
              wBody      :: [Stmt]
          }
    deriving (Show,Eq)

data ForInit = InitDecl Type String Expr -- int i = 0
             | InitExpr Expr             -- i = 0
             | NoInit                    -- no init
    deriving (Show,Eq)

data Expr = IExp   Integer
          | CExp   Char
          | FExp   Float
          | SExp   String
          | Assign   String Expr
          | BinExp   BinOp   Expr Expr
          | UnaryExp UnaryOp Expr
    deriving (Show,Eq)

data UnaryOp = Not           -- !a
             | MinusSign     -- -a
             | FirstCompl    -- ~a
             | IncrementPre  -- ++a
             | IncrementPost -- a++
             | DecrementPre  -- --a
             | DecrementPost -- a--
    deriving (Show,Eq)

data BinOp  = Addition       -- a +   b
            | Substraction   -- a -   b
            | Multiplication -- a *   b
            | Division       -- a /   b
            | Modulo         -- a %   b
            | BitShiftL      -- a <<  b
            | BitShiftR      -- a >>  b
            | Less           -- a <   b
            | Greater        -- a >   b
            | Leq            -- a <=  b
            | Geq            -- a >=  b
            | Equal          -- a ==  b
            | NotEqual       -- a !=  b
            | BitAnd         -- a &   b
            | BitXor         -- a ^   b
            | BitOr          -- a |   b
            | LogicAnd       -- a &&  b
            | LogicOr        -- a ||  b
            | PlusEq         -- a +=  b
            | MinusEq        -- a -=  b
            | MulEq          -- a *=  b
            | DivEq          -- a /=  b
            | ModEq          -- a %=  b
            | LShiftEq       -- a <<= b
            | RShiftEq       -- a >>= b
            | AndEq          -- a &=  b
            | XorEq          -- a ^=  b
            | OrEq           -- a |=  b
    deriving (Show,Eq)

data Type = TInt
          | TVoid
          | TFloat
          | TChar
    deriving (Show,Eq)
