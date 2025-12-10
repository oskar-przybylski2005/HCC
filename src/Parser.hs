{-# LANGUAGE FlexibleInstances #-}
module Parser where

import Lexer
import Text.Megaparsec
import Control.Monad (void)
import Data.Void

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

type Error = ParseErrorBundle [Lexer.LocatedToken] Void
type Parser a = Parsec Void Input a

type Input = [Lexer.LocatedToken]

type Program = [Function]

data Function = Function {
    retType :: Type,
    symbol  :: String,
    args    :: [Arg],
    body    :: [Stmt]
} deriving (Show,Eq)

type Arg  = (Type, String)

data Stmt = Ret Expr
          | Assign Type String Expr
    deriving (Show,Eq)

data Expr = IExp Integer
          | FExp Float
          | CExp Char
    deriving (Show,Eq)

data Type = TInt
          | TVoid
          | TFloat
          | TChar
    deriving (Show,Eq)


parseType :: Parser Type
parseType = choice
    [ TInt   <$ matchText "int"
    , TVoid  <$ matchText "void"
    , TChar  <$ matchText "char"
    , TFloat <$ matchText "float"
    ]

parseArg :: Parser Arg
parseArg = do
    typ    <- parseType
    skipSpaces
    symbol <- readToken TokenSymbol
    pure (typ, symbol)

parseArgs :: Parser [Arg]
parseArgs = do
    void $ parseToken TokenParL
    let sep = parseToken TokenComma *> skipSpaces
    args <- [] <$  matchText "void"
               <|> parseArg `sepBy` sep
    void $ parseToken TokenParR
    pure args

parseExpr :: Parser Expr
parseExpr = do
    let second str = str!!1
    expr <-  (IExp . read   <$> readToken TokenIntLit  )
         <|> (FExp . read   <$>(readToken TokenFloatLit
                            <|> readToken TokenSciLit  ))
         <|> (CExp . second <$> readToken TokenCharLit )
    pure expr

parseReturnStmt :: Parser Stmt
parseReturnStmt = do
    matchText "return"
    skipSpaces
    expr <- parseExpr
    void $ parseToken TokenSemi
    pure $ Ret expr

parseAssignStmt :: Parser Stmt
parseAssignStmt = do
    typ <- parseType
    skipSpaces
    symbol <- readToken TokenSymbol
    skipSpaces
    void $ parseToken TokenEq
    skipSpaces
    expr <- parseExpr
    skipSpaces
    void $ parseToken TokenSemi
    pure $ Assign typ symbol expr

parseStmt :: Parser Stmt
parseStmt = do
    skipWhitespace
    stmt <-  parseReturnStmt
         <|> parseAssignStmt
    skipWhitespace
    pure stmt

parseBlock :: Parser [Stmt]
parseBlock = do
    void $ parseToken TokenBraL

    skipWhitespace
    stmts <- many $ parseStmt
    skipWhitespace

    void $ parseToken TokenBraR

    pure stmts

parseFunction :: Parser Function
parseFunction = do
    rt <- parseType
    skipSpaces
    sb <- readToken TokenSymbol
    skipSpaces
    a  <- parseArgs
    skipNewLine
    b  <- parseBlock
    skipWhitespace
    pure Function {
        retType = rt,
        symbol  = sb,
        args    = a,
        body    = b
        }

runP :: String -> Input -> Parser a -> Either Error a
runP fileName input parser =
    case runParser parser fileName input of
        Right x  -> Right x
        Left err -> Left err

-- dodać parsowanie assignment stmt
-- dodać parsowanie ifow
--
-- PUBLIC API

parse :: String -> Input -> Either Error Program
parse fileName tokens =
    runP fileName tokens $
        many parseFunction
        <* parseToken TokenEnd
        <* eof

-- HELPERS

emitUnexpected :: MonadFail m => String -> m a
emitUnexpected s = fail $ "Unexpected symbol: " <> ("\""++s++"\"")

matchText :: String -> Parser ()
matchText s = void $ satisfy $ \t -> lText t == s

parseToken :: Lexer.Token -> Parser LocatedToken
parseToken t = satisfy $ \x -> lToken x == t

skipSpaces :: Parser ()
skipSpaces = do
    skipMany $ parseToken TokenSpace
    skipMany $ parseToken TokenTab

skipNewLine :: Parser ()
skipNewLine = void $ optional $ parseToken TokenNewLine

skipNewLines :: Parser ()
skipNewLines = skipMany $ parseToken TokenNewLine

readToken :: Lexer.Token -> Parser String
readToken t = do
    tok <- parseToken t
    pure $ lText tok

skipWhitespace :: Parser ()
skipWhitespace = skipMany $
        parseToken TokenSpace
    <|> parseToken TokenTab
    <|> parseToken TokenNewLine
