{-# LANGUAGE FlexibleInstances #-}
module Parser where

import Common
import Text.Megaparsec hiding (Token)

import Parser.Common
import Parser.AST
import Parser.FunctionParser
import Parser.StmtParser
import Parser.TypeParser
import Control.Monad

import Control.Monad.State.Strict
import qualified Data.Set as Set

-- PUBLIC API
parse :: String -> Input -> Either Error Program
parse fileName tokens =
    runP fileName tokens mainParser

runP :: String -> Input -> Parser a -> Either Error a
runP fileName input parser =
    let stateAction = runParserT parser fileName input
    in evalState stateAction Set.empty

mainParser :: Parser Program
mainParser = many parseHighLevelDeclaration
        <* parseToken TokenEnd
        <* eof

parseHighLevelDeclaration :: Parser HighLevelDeclaration
parseHighLevelDeclaration = 
          HighLevelTypeDefinition     <$> parseHLTypedef
      <|> HighLevelFunctionDefinition <$> try parseFunctionDefinition
      <|> HighLevelVariableD          <$> parseHighLevelVarD

parseHighLevelVarD :: Parser Statement
parseHighLevelVarD = do
    expr <- parseVarD
    void $ parseToken TokenSemi
    pure  expr



parseHLTypedef :: Parser TypeDef
parseHLTypedef = do
        matchText "typedef"
        typ <- parseType
        symbol <- readToken TokenSymbol
        void $ parseToken TokenSemi
        lift $ modify $ Set.insert symbol
        pure $ TypeDef typ symbol
    
