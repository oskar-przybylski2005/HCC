module Parser.StmtParser where

import Lexer
import Text.Megaparsec hiding (Token)
import Control.Monad (void)

import Parser.Common
import Parser.AST

import Parser.ExprParser
import Parser.TypeParser

parseReturnStmt :: Parser Stmt
parseReturnStmt = do
    matchText "return"
    skipSpaces
    expr <- parseExpr
    void $ parseToken TokenSemi
    pure $ Ret expr

parseBlock :: Parser [Stmt]
parseBlock = do
    void $ parseToken TokenBraL

    skipWhitespace
    stmts <- many parseStmt
    skipWhitespace

    void $ parseToken TokenBraR

    pure stmts

parseVarDeclStmt :: Parser Stmt
parseVarDeclStmt = do
    typ <- parseType
    skipSpaces
    symbol <- readToken TokenSymbol
    skipSpaces
    void $ parseToken TokenEq
    skipSpaces
    expr <- parseExpr
    skipSpaces
    void $ parseToken TokenSemi
    pure $ VarDecl typ symbol expr

parseIfStmt :: Parser Stmt
parseIfStmt = do
    void $ matchText "if"
    skipWhitespace
    cond <- parens parseExpr
    skipWhitespace
    body <- parseBlock <|>
            (:[]) <$> parseStmt
    skipWhitespace

    elseBody <- (do
        void $ matchText "else"
        skipWhitespace
        parseBlock
      ) <|> pure []

    pure IfStmt {
        iCondition=cond,
        iBodyBlock=body,
        iElseBlock=elseBody
    }
-- i = 5; funct() itp
parseExprStmt :: Parser Stmt
parseExprStmt = do
    expr <- parseExpr
    void $ parseToken TokenSemi
    pure $ ExprStmt expr

parseForInitDecl :: Parser ForInit
parseForInitDecl = do
    typ    <- parseType
    skipSpaces
    symbol <- readToken TokenSymbol
    skipSpaces
    void $ parseToken TokenEq
    skipSpaces
    InitDecl typ symbol <$> parseExpr

parseForAssignInit :: Parser ForInit
parseForAssignInit = do
    symbol <- readToken TokenSymbol
    skipSpaces
    void $ parseToken TokenEq
    skipSpaces
    InitExpr . Assign symbol <$> parseExpr

parseForInit :: Parser ForInit
parseForInit = do
        init <-  parseForInitDecl
             <|> parseForAssignInit
             <|> (NoInit   <$ parseToken TokenSemi)
        void $ optional $ parseToken TokenSemi
        pure init

parseForPar :: Parser (ForInit, Expr, Expr)
parseForPar = do
    void $ parseToken TokenParL
    skipSpaces
    init <- parseForInit
    cond <- parseExpr
    void $ parseToken TokenSemi
    step <- parseExpr
    skipSpaces
    void $ parseToken TokenParR
    pure (init, cond, step)

parseForStmt :: Parser Stmt
parseForStmt = do
    void $ matchText "for"
    skipSpaces
    (init, cond, step) <- parseForPar
    body <- parseBlock <|>
                 (:[]) <$> parseStmt

    pure ForStmt {
        fInit= init,
        fCondition= cond,
        fStep= step,
        fBody=body
    }

parseWhileStmt :: Parser Stmt
parseWhileStmt = do
    void $ matchText "while"
    skipWhitespace
    cond <- parseExpr
    body <- parseBlock <|>
                 (:[]) <$> parseStmt
    pure WhileStmt {
        wCond= cond,
        wBody= body
    }

parseFunctionCall :: Parser Stmt
parseFunctionCall = do
    symbol <- readToken TokenSymbol

    skipWhitespace
    void $ parseToken TokenParL

    args <- parseExpr `sepBy` parseToken TokenComma

    void $ parseToken TokenParR

    void $ optional $ parseToken TokenSemi

    pure $ FuncCall symbol args

parseStmt :: Parser Stmt
parseStmt = do
    skipWhitespace
    stmt <-  parseReturnStmt
         <|> parseFunctionCall
         <|> parseVarDeclStmt
         <|> parseIfStmt
         <|> parseForStmt
         <|> parseWhileStmt
         <|> parseExprStmt
    skipWhitespace
    pure stmt
