{-# LANGUAGE FlexibleInstances #-}
module Parser where

import Lexer
import Text.Megaparsec hiding (Token)

import Parser.Common
import Parser.AST
import Parser.FunctionParser

runP :: String -> Input -> Parser a -> Either Error a
runP fileName input parser =
    case runParser parser fileName input of
        Right x  -> Right x
        Left err -> Left err

-- PUBLIC API
parse :: String -> Input -> Either Error Program
parse fileName tokens =
    runP fileName tokens $
        skipWhitespace *>
        many parseFunctionDecl
        <* parseToken TokenEnd
        <* eof
