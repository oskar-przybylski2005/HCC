module Parser.FunctionParser where

import Common
import Text.Megaparsec hiding (Token)
import Control.Monad (void)

import Parser.Common
import Parser.AST

import Parser.TypeParser
import Parser.StmtParser

parseArg :: Parser Arg
parseArg = do
    typ    <- parseType
    symbol <- optional $ readToken TokenSymbol
    pure $ Arg typ symbol

parseArgs :: Parser [Arg]
parseArgs = do
    void $ parseToken TokenParL
    let sep = parseToken TokenComma 
    args <- [] <$  matchText "void"
               <|> parseArg `sepBy` sep
    void $ parseToken TokenParR
    pure args

parseFunctionDefinition :: Parser FunctionDefinition
parseFunctionDefinition = do
    strspec <- parseStorageSpecifier
    rt <- parseType
    sb <- readToken TokenSymbol
    a  <- parseArgs
    b <- parseBlock 
    pure FunctionDefinition {
        funcDStrSpec = strspec,
        funcDRetType = rt,
        funcDSymbol  = sb,
        funcDArgs    = a,
        funcDBody    = b
    }
