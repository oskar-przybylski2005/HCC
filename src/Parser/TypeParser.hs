module Parser.TypeParser where


import Text.Megaparsec hiding (Token)

import Parser.Common
import Parser.AST
import Common
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Set as Set

parseAlias :: Parser String
parseAlias = do
    symbol <- readToken TokenSymbol
    env    <- lift get
    if Set.member symbol env
        then pure symbol
        else customFailure $ UnknownTypeError symbol
        
parseField :: Parser StructField
parseField = do
    typ    <- parseType
    symbol <- readToken TokenSymbol
    void $ readToken TokenSemi
    pure $ StructField typ symbol

parseStruct :: Parser Type
parseStruct = do
    void $ matchText "struct"
    symbol <- readToken TokenSymbol
    void $ readToken TokenBraL
    fields <- many parseField
    void $ readToken TokenBraR
    pure $ TStruct symbol fields

parseType :: Parser Type
parseType = do
    typ <- choice
        [ TInt    <$ matchText "int"
        , TVoid   <$ matchText "void"
        , TChar   <$ matchText "char"
        , TFloat  <$ matchText "float"
        , TAlias  <$> parseAlias
        , parseStruct
        ]

    stars <- optional $ do
        many $ parseToken TokenStar 

    case stars of
        Just s -> pure $ foldl (\acc _ -> TPointer acc) typ s
        Nothing -> pure typ


