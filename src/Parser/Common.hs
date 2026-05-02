module Parser.Common(
    Error,
    Parser,
    Input,
    ParseError(..),
    readToken,
    parseToken,
    parseStorageSpecifier,
    parens,
    matchText,
    parseKeyword
) where

import Parser.AST
import Common

import Text.Megaparsec hiding (Token,State,ParseError)
import Control.Monad (void)

import Control.Monad.State.Strict
import qualified Data.Set as Set

type TypeEnv = Set.Set String

-- Megaparsec types
type Error  = ParseErrorBundle [LocatedToken] ParseError
type Parser = ParsecT ParseError Input (State TypeEnv)
type Input  = [LocatedToken]

-- Custom Errors

data ParseError
    = OtherError String
    | UnknownTypeError String
    deriving (Eq, Ord, Show)

instance ShowErrorComponent ParseError where
    showErrorComponent (OtherError msg) =
        "[!] Other error occured while parsing: " ++ msg
    showErrorComponent (UnknownTypeError typ) =
        "[!] Type " ++ typ ++ " is not defined."
-- HELPERS
matchText :: String -> Parser ()
matchText s = skipWhitespace $ void $ satisfy $ \t -> lText t == s

parseKeyword :: String -> Parser ()
parseKeyword kw = skipWhitespace $ void $ satisfy $ \t -> 
    lToken t == TokenKeyWord && lText t == kw

parseToken :: Token -> Parser LocatedToken
parseToken t = skipWhitespace $ satisfy $ \x -> lToken x == t

readToken :: Token -> Parser String
readToken t = skipWhitespace $ do
    tok <- parseToken t
    pure $ lText tok

consumeWhitespace :: Parser ()
consumeWhitespace = skipMany $
        parseToken TokenSpace
    <|> parseToken TokenTab

skipWhitespace :: Parser a -> Parser a 
skipWhitespace p = p <* consumeWhitespace

-- extracts content from within parenthesis
parens :: Parser a -> Parser a
parens p = do
    void $ parseToken TokenParL
    x <- p
    void $ parseToken TokenParR
    pure x

parseStorageSpecifier :: Parser StorageSpecifier
parseStorageSpecifier = choice
    [ Static   <$ parseKeyword "static"
    , Extern   <$ parseKeyword "extern"
    , Auto     <$ parseKeyword "auto"
    , Register <$ parseKeyword "register"
    ] <|> pure ScNone

