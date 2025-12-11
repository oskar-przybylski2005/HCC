module Parser.Common where

import Lexer

import Text.Megaparsec hiding (Token)
import Control.Monad (void)
import Data.Void

-- Megaparsec types
type Error = ParseErrorBundle [LocatedToken] Void
type Parser a = Parsec Void Input a
type Input = [LocatedToken]

-- HELPERS
emitUnexpected :: MonadFail m => String -> m a
emitUnexpected s = fail $ "Unexpected symbol: " <> ("\""++s++"\"")

matchText :: String -> Parser ()
matchText s = void $ satisfy $ \t -> lText t == s

parseToken :: Token -> Parser LocatedToken
parseToken t = satisfy $ \x -> lToken x == t

skipSpaces :: Parser ()
skipSpaces = do
    skipMany $ parseToken TokenSpace
    skipMany $ parseToken TokenTab

skipNewLine :: Parser ()
skipNewLine = void $ optional $ parseToken TokenNewLine

skipNewLines :: Parser ()
skipNewLines = skipMany $ parseToken TokenNewLine

readToken :: Token -> Parser String
readToken t = do
    tok <- parseToken t
    pure $ lText tok

skipWhitespace :: Parser ()
skipWhitespace = skipMany $
        parseToken TokenSpace
    <|> parseToken TokenTab
    <|> parseToken TokenNewLine

-- extracts content from within parenthesis
parens :: Parser a -> Parser a
parens p = do
    void $ parseToken TokenParL
    skipSpaces
    x <- p
    skipSpaces
    void $ parseToken TokenParR
    pure x
