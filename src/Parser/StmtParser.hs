module Parser.StmtParser(
    parseBlock,
    parseVarD
) where

import Common
import Text.Megaparsec hiding (Token)
import Control.Monad (void)

import Parser.Common
import Parser.AST

import Parser.ExprParser
import Parser.TypeParser


parseBlock :: Parser [Statement]
parseBlock = do
    void $ parseToken TokenBraL

    stmts <- many parseStmt

    void $ parseToken TokenBraR

    pure stmts


parseVarD :: Parser Statement
parseVarD = do
    storageSpecifier <- parseStorageSpecifier
    typ <- parseType
    symbol <- readToken TokenSymbol
    expr <- optional $ do
        void $ parseToken TokenEq
        parseExpr
    pure $ case expr of
        Just e  ->  VarDefinitionStmt storageSpecifier typ symbol e
        Nothing ->  VarDeclarationStmt storageSpecifier typ symbol

-- private
parseReturnStmt :: Parser Statement
parseReturnStmt = do
    parseKeyword "return"
    expr <- optional parseExpr
    void $ parseToken TokenSemi
    pure $ Ret expr

parseIfStmt :: Parser Statement
parseIfStmt = do
    parseKeyword "if"
    cond <- parens parseExpr
    body <- parseBlock <|>
            (:[]) <$> parseStmt

    elseBody <- (do
        parseKeyword "else"
        parseBlock
      ) <|> pure []

    pure IfStmt {
        iCondition=cond,
        iBodyBlock=body,
        iElseBlock=elseBody
    }
-- i = 5; funct() itp
parseExprStmt :: Parser Statement
parseExprStmt = ExprStmt <$> parseExpr

parseForStmt :: Parser Statement
parseForStmt = do
    void $ parseKeyword "for"

    void $ parseToken TokenParL

    init <- optional $ try parseVarD <|> parseExprStmt
    void $ parseToken TokenSemi

    cond <- optional parseExpr
    void $ parseToken TokenSemi

    step <- optional parseExpr

    void $ parseToken TokenParR

    body <- optional $ parseBlock
                       <|> (:[]) <$> parseStmt

    pure ForStmt {
        fInit= init,
        fCondition= cond,
        fStep= step,
        fBody=body
    }

parseWhileStmt :: Parser Statement
parseWhileStmt = do
    void $ parseKeyword "while"
    cond <- parseExpr
    body <- parseBlock <|>
                 (:[]) <$> parseStmt

    pure WhileStmt {
        wCond= cond,
        wBody= body
    }

parseNullStmt :: Parser Statement
parseNullStmt = do
    void $ parseToken TokenSemi
    pure NullStmt

parseStmt :: Parser Statement
parseStmt =  parseReturnStmt
         <|> try parseVarD
         <|> parseIfStmt
         <|> parseForStmt
         <|> parseWhileStmt
         <|> parseExprStmt
         <|> parseNullStmt
