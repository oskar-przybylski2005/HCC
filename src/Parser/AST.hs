module Parser.AST (
  Program,
  HighLevelDeclaration(..),
  TypeDef(..),
  FunctionDefinition(..),
  StorageSpecifier(..),
  Statement(..),
  StructField(..),
  Type(..),
  UnaryOp(..),
  Expression(..),
  BinOp(..),
  Arg(..),
  prettyPrint
) where

import Data.Tree (Tree(..))

-- AST types
type Program = [HighLevelDeclaration]


data HighLevelDeclaration = HighLevelFunctionDefinition FunctionDefinition
                          | HighLevelTypeDefinition TypeDef
                          | HighLevelVariableD Statement -- VarD 
                          
  deriving (Show,Eq)

data TypeDef = TypeDef Type String
  deriving (Show,Eq)

data FunctionDefinition = FunctionDefinition {
      funcDStrSpec :: StorageSpecifier,
      funcDRetType :: Type,
      funcDSymbol  :: String,
      funcDArgs    :: [Arg],
      funcDBody    :: [Statement]
} deriving (Show,Eq)

data StorageSpecifier = Static
                      | Extern
                      | Auto
                      | Register
                      | ScNone   -- default local/global var
    deriving (Show,Eq)
    
data Arg  = Arg Type (Maybe String)
    deriving (Show,Eq)

data Statement 
          = Ret (Maybe Expression)
          | VarDeclarationStmt{
            vdecStorage :: StorageSpecifier,
            vdecType    :: Type,
            vdecName    :: String
          }
          | VarDefinitionStmt{
             vdefStorage   :: StorageSpecifier,
             vdefType      :: Type,
             vdefName      :: String,
             vdefValue     :: Expression
          }
          | ExprStmt Expression
          | IfStmt {
             iCondition  ::  Expression,
             iBodyBlock  :: [Statement],
             iElseBlock  :: [Statement]
          }
          | ForStmt{
              fInit      ::  Maybe Statement, -- VarD or Assignment
              fCondition ::  Maybe Expression,
              fStep      ::  Maybe Expression,
              fBody      ::  Maybe [Statement]
          }
          | WhileStmt{
              wCond      ::  Expression,
              wBody      :: [Statement]
          }
          | NullStmt -- ;;;;
    deriving (Show,Eq)

data Expression 
          = IntegerExp      Integer
          | CharExp         Char
          | FloatExp        Float
          | StringExp       String
          | FuncCall        String [Expression]
          | BinExp          BinOp   Expression Expression
          | UnaryExp        UnaryOp Expression
          | AssigmentExp    Expression Expression
    deriving (Show,Eq)

data UnaryOp = Not              -- !a
             | MinusSign        -- -a
             | FirstCompl       -- ~a
             | IncrementPre     -- ++a
             | IncrementPost    -- a++
             | DecrementPre     --  --a
             | DecrementPost    -- a--
             | AdressOf         -- &a
             | Deref            -- *a
             | Element    Expression  -- a[x]
             | RefField   String-- a -> x
             | Field      String-- a . x
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

data StructField = StructField Type String
    deriving (Show,Eq)

data Type = TInt
          | TVoid
          | TFloat
          | TChar
          | TPointer Type
          | TAlias   String
          | TStruct  String [StructField]
    deriving (Show,Eq)

prettyPrint :: Show a => a -> IO ()
prettyPrint = putStrLn . compactDrawTree . simplifyTree . filterTree "NullStmt" . parseShow . show

compactDrawTree :: Tree String -> String
compactDrawTree = unlines . compactDraw

compactDraw :: Tree String -> [String]
compactDraw (Node x ts) = x : compactDrawSubTrees ts

compactDrawSubTrees :: [Tree String] -> [String]
compactDrawSubTrees [] = []
compactDrawSubTrees [t] = shift "`- " "   " (compactDraw t)
compactDrawSubTrees (t:ts) = shift "+- " "|  " (compactDraw t) ++ compactDrawSubTrees ts

shift :: String -> String -> [String] -> [String]
shift first other (x:xs) = (first ++ x) : map (other ++) xs
shift _ _ [] = []

simplifyTree :: Tree String -> Tree String
simplifyTree (Node root [child])
    | root `elem` [
            "HighLevelFunctionDefinition", 
            "HighLevelTypeDefinition", 
            "HighLevelVariableD", 
            "ExprStmt", 
            "BinExp", 
            "UnaryExp", 
            "IntegerExp", 
            "CcharExp", 
            "FloatExp", 
            "StringExp",
            "Just"
        ] =
        simplifyTree child
simplifyTree (Node root children) =
    Node root (map simplifyTree children)

filterTree :: String -> Tree String -> Tree String
filterTree toRemove (Node val children) = 
    Node val [filterTree toRemove child | child@(Node v _) <- children, v /= toRemove]

parseShow :: String -> Tree String
parseShow s = fst $ parseNode $ tokenize s

tokenize :: String -> [String]
tokenize "" = []
tokenize s = case lex s of
               [("", _)] -> []
               [(tok, rest)] -> tok : tokenize rest
               _ -> []

parseNode :: [String] -> (Tree String, [String])
parseNode [] = (Node "" [], [])
parseNode ("(":ts) =
    let (tree, ts') = parseNode ts
    in case ts' of
         ")":rest -> (tree, rest)
         _ -> (tree, ts')
parseNode ("[":ts) =
    let (nodes, ts') = parseCommaSep "]" ts
    in (Node "[]" nodes, ts')
parseNode (t:"{":ts) | isCon t =
    let (nodes, ts') = parseRecord ts
    in (Node t nodes, ts')
parseNode ("-":n:ts) | not (null n) && head n >= '0' && head n <= '9' = 
    (Node ("-" ++ n) [], ts)
parseNode (t:ts) | isCon t =
    let (args, ts') = parseArgs ts
    in (Node t args, ts')
parseNode (t:ts) = (Node t [], ts)

isCon :: String -> Bool
isCon (c:_) = c >= 'A' && c <= 'Z'
isCon _ = False

parseArgs :: [String] -> ([Tree String], [String])
parseArgs [] = ([], [])
parseArgs ts@(t:_) | t `elem` [")", "]", "}", ",", "="] = ([], ts)
parseArgs ts =
    let (arg, ts') = parseNode ts
        (args, ts'') = parseArgs ts'
    in (arg : args, ts'')

parseCommaSep :: String -> [String] -> ([Tree String], [String])
parseCommaSep _ [] = ([], [])
parseCommaSep end ts@(t:_) | t == end = ([], tail ts)
parseCommaSep end ts =
    let (node, ts') = parseNode ts
    in case ts' of
         ",":ts'' -> let (nodes, ts''') = parseCommaSep end ts''
                     in (node : nodes, ts''')
         t:ts'' | t == end -> ([node], ts'')
         _ -> ([node], ts')

parseRecord :: [String] -> ([Tree String], [String])
parseRecord [] = ([], [])
parseRecord ("}":ts) = ([], ts)
parseRecord (field:"=":ts) =
    let (valNode, ts') = parseNode ts
        node = Node (field ++ " = " ++ rootLabel valNode) (subForest valNode)
    in case ts' of
         ",":ts'' -> let (nodes, ts''') = parseRecord ts''
                     in (node : nodes, ts''')
         "}":ts'' -> ([node], ts'')
         _ -> ([node], ts')
parseRecord ts = parseCommaSep "}" ts