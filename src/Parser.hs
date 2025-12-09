{-# LANGUAGE FlexibleInstances #-} -- for VisualStream impl for Lexer.Token (alias Input)
module Parser where

import Lexer
import Text.Megaparsec
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

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

type Parser a = Parsec Error Input a

type Error = T.Text
instance ShowErrorComponent Error where
  showErrorComponent = T.unpack

type Input = [Lexer.Token]

data Type = TInt
          | TVoid
          | TFloat
          | TChar
    deriving (Show,Eq)


runTypeParser :: Parser a -> Input -> Either Error a
runTypeParser parser input =
    case runParser parser "" input of
        Left e  -> Left $ T.pack $ errorBundlePretty e
        Right x -> Right x

parseType :: Parser Type
parseType = do
    t <- anySingle
    case t of
        TokenKeyWord "int"   -> pure TInt
        TokenKeyWord "float" -> pure TFloat
        TokenKeyWord "char"  -> pure TChar
        TokenKeyWord "void"  -> pure TVoid
        _ -> fail $ "Unexpected symbol: " <> show t
