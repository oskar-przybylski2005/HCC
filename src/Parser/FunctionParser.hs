module Parser.FunctionParser where

import Lexer
import Text.Megaparsec hiding (Token)
import Control.Monad (void)

import Parser.Common
import Parser.AST

import Parser.TypeParser
import Parser.StmtParser

parseArg :: Parser Arg
parseArg = do
    typ    <- parseType
    skipSpaces
    symbol <- readToken TokenSymbol
    pure (typ, symbol)

parseArgs :: Parser [Arg]
parseArgs = do
    void $ parseToken TokenParL
    skipSpaces
    let sep = parseToken TokenComma *> skipSpaces
    args <- [] <$  matchText "void"
               <|> parseArg `sepBy` sep
    skipSpaces
    void $ parseToken TokenParR
    pure args


parseFunctionDecl :: Parser FunctionDecl
parseFunctionDecl = do
    rt <- parseType
    skipSpaces
    sb <- readToken TokenSymbol
    skipSpaces
    a  <- parseArgs
    skipWhitespace
    b  <- parseBlock
    skipWhitespace
    pure FunctionDecl {
        funcRetType = rt,
        funcSymbol  = sb,
        funcArgs    = a,
        funcBody    = b
        }

